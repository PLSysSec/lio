{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |This module abstracts the basic 'FileHandle' methods provided by
-- the system library, and provides an 'LHandle' (Labeled Handle) type
-- that can be manipulated from within the 'LIO' Monad.  Two lower
-- level functions, 'mkDir' and 'mkLHandle' may be useful for
-- functions that wish to open file names that are not relative to
-- 'rootDir'.  (There is no notion of changeable current working
-- directory in the 'LIO' Monad.)
module LIO.Handle (DirectoryOps(..)
                  , CloseOps (..)
                  , HandleOps (..)
                  , LHandle
                  , mkDir, mkLHandle
                  , readFile, writeFile
                  , createDirectoryP, openFileP, writeFileP
                  ) where

import LIO.TCB
import LIO.FS

import Prelude hiding (readFile, writeFile)
import Control.Exception
import qualified Data.ByteString.Lazy as L
import qualified System.Directory as IO
import qualified System.IO as IO
import qualified System.IO.Error as IO

class (Monad m) => DirectoryOps h m | m -> h where
    getDirectoryContents :: FilePath -> m [FilePath]
    createDirectory      :: FilePath -> m ()
    openFile             :: FilePath -> IO.IOMode -> m h

class (Monad m) => CloseOps h m where
    hClose               :: h -> m ()

class (CloseOps h m) => HandleOps h b m where
    hGet            :: h -> Int -> m b
    hGetNonBlocking :: h -> Int -> m b
    hGetContents    :: h -> m b
    hPut            :: h -> b -> m ()
    hPutStrLn       :: h -> b -> m ()

instance DirectoryOps IO.Handle IO where
    getDirectoryContents = IO.getDirectoryContents
    createDirectory      = IO.createDirectory
    openFile             = IO.openBinaryFile

instance CloseOps IO.Handle IO where
    hClose               = IO.hClose

instance HandleOps IO.Handle L.ByteString IO where
    hGet            = L.hGet
    hGetNonBlocking = L.hGetNonBlocking
    hGetContents    = L.hGetContents
    hPut            = L.hPut
    hPutStrLn h s   = L.hPut h $ L.append s $ L.singleton 0xa

data LHandle l h = LHandleTCB l h

instance (Label l) => DirectoryOps (LHandle l IO.Handle) (LIO l s) where
    getDirectoryContents d  = do
      node <- lookupNode NoPrivs rootDir d False
      rtioTCB $ getDirectoryContentsNode node
    createDirectory path    = do
      l <- currentLabel
      mkDir NoPrivs l rootDir path
    openFile path mode      = do
      l <- currentLabel
      mkLHandle NoPrivs l rootDir path mode

instance (Label l) => CloseOps (LHandle l IO.Handle) (LIO l s) where
    hClose (LHandleTCB l h) = guardio l >> rtioTCB (hClose h)

instance (Label l, CloseOps (LHandle l h) (LIO l s), HandleOps h b IO)
    => HandleOps (LHandle l h) b (LIO l s) where
    hGet (LHandleTCB l h) n       = guardio l >> rtioTCB (hGet h n)
    hGetNonBlocking (LHandleTCB l h) n =
                                  guardio l >> rtioTCB (hGetNonBlocking h n)
    hGetContents (LHandleTCB l h) = guardio l >> rtioTCB (hGetContents h)
    hPut (LHandleTCB l h) s       = guardio l >> rtioTCB (hPut h s)
    hPutStrLn (LHandleTCB l h) s  = guardio l >> rtioTCB (hPutStrLn h s)


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
                                 -> IO.IOMode -- ^Mode of handle
                                 -> LIO l s (LHandle l IO.Handle)
mkLHandle priv l start path mode = do
  cleario l
  name <- lookupName priv start path
  dirlabel <- ioTCB $ labelOfName name
  ptaintio priv dirlabel
  newl <- currentLabel
  mnode <- ioTCB $ tryPred IO.isDoesNotExistError (nodeOfName name)
  case (mnode, mode) of
    (Right node, _) ->
        do nodel <- ioTCB $ labelOfNode node
           let hl = if mode == IO.ReadMode
                    then l `lub` newl `lub` nodel
                    else nodel
           cleario hl
           h <- rtioTCB $ openNode node mode
           return $ LHandleTCB hl h
    (Left e, IO.ReadMode) -> throwL e
    _ -> do pguardio priv dirlabel
            cleario l           -- lookupName may have changed label
            (h, new) <- rtioTCB $ mkNodeReg mode l
            mn <- rtioTCB $ tryPred IO.isAlreadyExistsError
                  (linkNode new name `onException` hClose h)
            case mn of
              Right _ -> return $ LHandleTCB l h
              Left _  -> mkLHandle priv l name "" mode

readFile      :: (DirectoryOps h m, HandleOps h b m) => FilePath -> m b
readFile path = openFile path IO.ReadMode >>= hGetContents

writeFile               :: (DirectoryOps h m, HandleOps h b m,
                            OnExceptionTCB m) => FilePath -> b -> m ()
writeFile path contents = bracketTCB (openFile path IO.WriteMode) hClose
                          (flip hPut contents)

createDirectoryP            :: (Priv l p) => p -> FilePath -> LIO l s ()
createDirectoryP privs path = do
  l <- currentLabel
  mkDir privs l rootDir path

writeFileP  :: (Priv l p, HandleOps IO.Handle b IO) =>
               p -> FilePath -> b -> LIO l s ()
writeFileP privs path contents =
  bracketTCB (openFileP privs path IO.WriteMode) hClose
             (\h -> hPut h contents)

openFileP :: (Priv l p) =>
             p -> FilePath -> IOMode -> LIO l s (LHandle l IO.Handle)
openFileP privs path mode = do
  l <- currentLabel
  mkLHandle privs l rootDir path mode

