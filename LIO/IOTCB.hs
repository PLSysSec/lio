{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
-- {-# OPTIONS_GHC -fglasgow-exts #-}

module LIO.IOTCB {- (
                   LIORef, newLIORef, labelOfLIORef
                 , readLIORef, writeLIORef, atomicModifyLIORef
                 , IOMode, IsHandle(..)
                 ) -}
    where

import LIO.Armor
import LIO.TCB

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IORef
import Data.Typeable
import Data.Word
import System.Directory
import System.IO (IOMode, FilePath, stderr)
import qualified System.IO as IO
import System.FilePath

import Text.Printf

import Data.Digest.Pure.SHA


--
-- Misc wrappers
--


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
    openBinaryFile :: FilePath -> IOMode -> m h
    hClose :: h -> m ()

instance IsHandleOpen IO.Handle IO where
    openBinaryFile = IO.openBinaryFile
    hClose = IO.hClose

class (IsHandleOpen h m) => IsHandle h b m where
    hGet :: h -> Int -> m b
    hGetNonBlocking :: h -> Int -> m b
    hPutStr :: h -> b -> m ()
    hPutStrLn :: h -> b -> m ()

instance IsHandle IO.Handle B.ByteString IO where
    hGet = B.hGet
    hGetNonBlocking = B.hGetNonBlocking
    hPutStr = B.hPutStr
    hPutStrLn = B.hPutStrLn

data LHandle l h = LHandleTCB l h

instance (Label l, IsHandleOpen (LHandle l h) (LIO l s), IsHandle h b IO)
    => IsHandle (LHandle l h) b (LIO l s) where
    hGet (LHandleTCB l h) n = guardio l >> ioTCB (hGet h n)
    hGetNonBlocking (LHandleTCB l h) n =
        guardio l >> ioTCB (hGetNonBlocking h n)
    hPutStr (LHandleTCB l h) s = guardio l >> ioTCB (hPutStr h s)
    hPutStrLn (LHandleTCB l h) s = guardio l >> ioTCB (hPutStr h s)

instance (Label l, IsHandleOpen h IO)
    => IsHandleOpen (LHandle l h) (LIO l s) where
    openBinaryFile = undefined
    hClose (LHandleTCB l h) = guardio l >> ioTCB (hClose h)


hlabelOf                  :: (Label l) => LHandle l h -> l
hlabelOf (LHandleTCB l h) = l


--
-- Labeled storage
--

labelStr2Path   :: String -> FilePath
labelStr2Path l = case armor32 $ bytestringDigest $ sha224 $ LC.pack $ l of
                 c1:c2:c3:rest -> ((c1:[]) </> (c2:c3:[]) </> rest)

mkLabelDir   :: Label l => l -> IO String
mkLabelDir l = do
  let label = show l
      dir = labelStr2Path label
      labelfile = (dir </> ".label")
  label' <- readFile labelfile
  if label' == label
    then return dir
    else IO.hPutStrLn stderr ("file " ++ labelfile
                           ++ " should contain:\n" ++ label) >>
               error "fs corruption"


{-
ls = "LabeledStorage"
init   :: (Label l) => l -> LIO l s ()
init l = rethrowTCB $ ioTCB $ do
           createDirectory ls
           changeWorkingDirectory ls
           let root = label2path l
           createDirectoryIfMissing True root
           createSymbolicLink root "root" 
-}
         

--
-- Crap
--


lgetLine :: (Label l, Typeable s) => LIO l s String
lgetLine = ioTCB getLine
lputStr x = ioTCB $ putStr x
lputStrLn x = ioTCB $ putStrLn x

