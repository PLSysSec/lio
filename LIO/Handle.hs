{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}

module LIO.Handle where

import LIO.TCB

import qualified Data.ByteString.Lazy as L
import qualified System.Directory as IO
import qualified System.IO as IO

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

instance (Label l, DirectoryOps h IO) =>
    DirectoryOps (LHandle l h) (LIO l s) where
        getDirectoryContents    = undefined
        createDirectory         = undefined
        openFile                = undefined
        hClose (LHandleTCB l h) = guardio l >> rtioTCB (hClose h)


hlabelOf                  :: (Label l) => LHandle l h -> l
hlabelOf (LHandleTCB l h) = l

