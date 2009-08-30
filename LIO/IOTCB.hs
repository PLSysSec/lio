{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fglasgow-exts #-}

module LIO.IOTCB {- (
                   LIORef, newLIORef, labelOfLIORef
                 , readLIORef, writeLIORef, atomicModifyLIORef
                 , IOMode, IsHandle(..)
                 ) -}
    where

import LIO.Armor
import LIO.TCB
import LIO.TmpFile

import Prelude hiding (catch)
import Control.Exception (throwIO, catch, Exception(..), IOException(..))
import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IORef
import Data.Typeable (Typeable)
import Data.Word (Word8)
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.FilePath (FilePath(..), (</>))
import System.IO (IOMode(..), FilePath(..), stderr)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import System.Posix.Directory
import System.Posix.Files
import System.Posix.IO
import System.Time (ClockTime(..), getClockTime)

import Data.Digest.Pure.SHA
import qualified System.IO.Cautious as CIO


--
-- LIOref -- labeled IOref
--

data LIORef l a = LIORefTCB l (IORef a)

newLIORef :: (Label l, Typeable s) => l -> a -> LIO l s (LIORef l a)
newLIORef l a = do
  guardio l
  ior <- ioTCB $ newIORef a
  return $ LIORefTCB l ior

labelOfLIORef :: (Label l) => LIORef l a -> l
labelOfLIORef (LIORefTCB l _) = l

readLIORef :: (Label l, Typeable s) => LIORef l a -> LIO l s a
readLIORef (LIORefTCB l r) = do
  taintio l
  val <- ioTCB $ readIORef r
  return val

writeLIORef :: (Label l, Typeable s) => LIORef l a -> a -> LIO l s ()
writeLIORef (LIORefTCB l r) a = do
  guardio l
  ioTCB $ writeIORef r a

atomicModifyLIORef :: (Label l, Typeable s) =>
                      LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORef (LIORefTCB l r) f = do
  guardio l
  ioTCB $ atomicModifyIORef r f

--
-- File operations
--

class IsHandleOpen h m where
    openFile :: FilePath -> IOMode -> m h
    hClose :: h -> m ()

instance IsHandleOpen IO.Handle IO where
    openFile = IO.openBinaryFile
    hClose = IO.hClose

class (IsHandleOpen h m) => IsHandle h b m where
    hGet :: h -> Int -> m b
    hGetNonBlocking :: h -> Int -> m b
    hPut :: h -> b -> m ()
    hPutStrLn :: h -> b -> m ()

instance IsHandle IO.Handle L.ByteString IO where
    hGet = L.hGet
    hGetNonBlocking = L.hGetNonBlocking
    hPut = L.hPut
    hPutStrLn h s = L.hPut h $ L.append s $ L.singleton 0xa

data LHandle l h = LHandleTCB l h

instance (Label l, IsHandleOpen (LHandle l h) (LIO l s), IsHandle h b IO)
    => IsHandle (LHandle l h) b (LIO l s) where
    hGet (LHandleTCB l h) n = guardio l >> rtioTCB (hGet h n)
    hGetNonBlocking (LHandleTCB l h) n =
        guardio l >> rtioTCB (hGetNonBlocking h n)
    hPut (LHandleTCB l h) s = guardio l >> rtioTCB (hPut h s)
    hPutStrLn (LHandleTCB l h) s = guardio l >> rtioTCB (hPutStrLn h s)

instance (Label l, IsHandleOpen h IO)
    => IsHandleOpen (LHandle l h) (LIO l s) where
    openFile = undefined
    hClose (LHandleTCB l h) = guardio l >> rtioTCB (hClose h)


hlabelOf                  :: (Label l) => LHandle l h -> l
hlabelOf (LHandleTCB l h) = l

--
-- Labeled storage
--

data LIOerr
    = LioCorruptLabel String String -- ^File Containing Label is Corrupt
      deriving (Show, Typeable)
instance Exception LIOerr
            

labelStr2Path   :: String -> FilePath
labelStr2Path l = case armor32 $ bytestringDigest $ sha224 $ LC.pack $ l of
                 c1:c2:c3:rest -> ((c1:[]) </> (c2:c3:[]) </> rest)

strictReadFile   :: FilePath -> IO LC.ByteString
strictReadFile f = IO.withFile f ReadMode readit
    where readit h = do
            size <- IO.hFileSize h
            LC.hGet h $ fromInteger size

labelfname = ".label"
mkLabelDir   :: Label l => l -> IO String
mkLabelDir l =
    let label = show l
        path = labelStr2Path label
        file = path </> labelfname
        correct = LC.pack $ label
        checkfile = do
          contents <- strictReadFile file
          unless (contents == correct) $
                 throwIO (LioCorruptLabel file $ LC.unpack correct)
        nosuch e = if IO.isDoesNotExistError e
                   then do let tpath = path ++ "~"
                               tfile = tpath </> labelfname
                           createDirectoryIfMissing True tpath
                           putStrLn $ "creating " ++ tfile
                           CIO.writeFileL tfile correct
                           rename tpath path
                   else throwIO e
    in do checkfile `catch` nosuch
          return path


ls = "labeledStorage"
lsinitTCB   :: (Label l) => l -> LIO l s ()
lsinitTCB l = rtioTCB $ changeWorkingDirectory ls `catch` setup
    where
      setup :: IOException -> IO ()
      setup e = do
        createDirectoryIfMissing True ls
        changeWorkingDirectory ls
        root <- mkLabelDir l
        createSymbolicLink root "root" 


--
-- Crap
--


lgetLine :: (Label l, Typeable s) => LIO l s String
lgetLine = ioTCB getLine
lputStr x = ioTCB $ putStr x
lputStrLn x = ioTCB $ putStrLn x

