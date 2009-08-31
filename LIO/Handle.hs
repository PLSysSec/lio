{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}

module LIO.Handle (DirectoryOps(..)
                  , HandleOps (..)
                  , LHandle
                  , mkDir, mkLHandle
                  ) where

import LIO.TCB
import LIO.FS

import Control.Exception
import qualified Data.ByteString.Lazy as L
import qualified System.Directory as IO
import qualified System.IO as IO
import qualified System.IO.Error as IO

class DirectoryOps h m where
    getDirectoryContents :: FilePath -> m [FilePath]
    createDirectory      :: FilePath -> m ()
    openFile             :: FilePath -> IO.IOMode -> m h
    hClose               :: h -> m ()

instance DirectoryOps IO.Handle IO where
    getDirectoryContents = IO.getDirectoryContents
    createDirectory      = IO.createDirectory
    openFile             = IO.openBinaryFile
    hClose               = IO.hClose

class (DirectoryOps h m) => HandleOps h b m where
    hGet            :: h -> Int -> m b
    hGetNonBlocking :: h -> Int -> m b
    hPut            :: h -> b -> m ()
    hPutStrLn       :: h -> b -> m ()

instance HandleOps IO.Handle L.ByteString IO where
    hGet            = L.hGet
    hGetNonBlocking = L.hGetNonBlocking
    hPut            = L.hPut
    hPutStrLn h s   = L.hPut h $ L.append s $ L.singleton 0xa

data LHandle l h = LHandleTCB l h

instance (Label l, DirectoryOps (LHandle l h) (LIO l s), HandleOps h b IO)
    => HandleOps (LHandle l h) b (LIO l s) where
    hGet (LHandleTCB l h) n      = guardio l >> rtioTCB (hGet h n)
    hGetNonBlocking (LHandleTCB l h) n =
                                 guardio l >> rtioTCB (hGetNonBlocking h n)
    hPut (LHandleTCB l h) s      = guardio l >> rtioTCB (hPut h s)
    hPutStrLn (LHandleTCB l h) s = guardio l >> rtioTCB (hPutStrLn h s)

instance (Label l) => DirectoryOps (LHandle l IO.Handle) (LIO l s) where
        getDirectoryContents    = undefined
        createDirectory path    = do
          l <- labelOfio
          mkDir NoPrivs l rootDir path
        openFile path mode      = do
          l <- labelOfio
          mkLHandle NoPrivs l rootDir path mode
        hClose (LHandleTCB l h) = guardio l >> rtioTCB (hClose h)


hlabelOf                  :: (Label l) => LHandle l h -> l
hlabelOf (LHandleTCB l h) = l



mkDir                   :: (Priv l p) =>
                           p        -- ^Privileges
                        -> l        -- ^Label for the new directory
                        -> Name     -- ^Start point
                        -> FilePath -- ^Name to create
                        -> LIO l s () 
mkDir priv l start path = do
  -- No privs when checking clearance, as we assume it was lowered for a reason
  cleario l                     
  name <- lookupName priv start path
  dirlabel <- ioTCB $ labelOfName name
  pguardio priv dirlabel
  new <- ioTCB $ mkNodeDir l
  rtioTCB $ linkNode new name
  return ()

mkLHandle                        :: (Priv l p) =>
                                    p -- ^Privileges to minimize taint
                                 -> l -- ^Label if new file is created
                                 -> Name -- ^Starting point of pathname
                                 -> FilePath -- ^Path of file relative to prev
                                 -> IOMode   -- ^Mode of handle
                                 -> LIO l s (LHandle l IO.Handle)
mkLHandle priv l start path mode = do
  cleario l
  name <- lookupName priv start path
  dirlabel <- ioTCB $ labelOfName name
  ptaintio priv dirlabel
  mnode <- ioTCB $ tryPred IO.isDoesNotExistError (nodeOfName name)
  case (mnode, mode) of
    (Right node, _) ->
        do nl <- ioTCB $ labelOfNode node
           cleario nl
           let hl = if mode == IO.ReadMode && leq nl l then l else nl
           h <- rtioTCB $ openNode node mode
           return $ LHandleTCB hl h
    (Left e, IO.ReadMode) -> throwL e
    _ -> do guardio dirlabel
            (h, new) <- rtioTCB $ mkNodeReg mode l
            mn <- rtioTCB $ tryPred IO.isAlreadyExistsError
                  (linkNode new name `onException` hClose h)
            case mn of
              Right _ -> return $ LHandleTCB l h
              Left _  -> mkLHandle priv l name "" mode
                        
