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

import LIO.TCB
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.IORef
import Data.Typeable
import Data.Word
import System.Directory
import System.IO (IOMode, FilePath)
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
-- Base-32 utility functions
--

a2b :: UArray Word8 Char
a2b = listArray (0, 31) $ do c <- ['a'..'z'] ++ ['0' .. '9']
                             guard $ not $ elem c "lo01"
                             return c

armor32 :: B.ByteString -> String
armor32 s = doit 0 $ B.unpack s
    where
      doit _ [] = []
      doit skip s@(c1:s1) =
          let hi = shift c1 (skip - 3) .&. 0x1f
              lo = if skip <= 3 || s1 == []
                   then 0
                   else shift (head s1) (skip - 11)
              c = a2b ! (hi .|. lo)
          in if skip >= 3
             then c : doit (skip - 3) s1
             else c : doit (skip + 5) s

inval = -1 :: Word8
b2a :: UArray Char Word8
b2a = accumArray (\a b -> b) inval (chr 0, chr 255)
      [(y, x) | (x, y) <- assocs a2b]

dearmor32 :: String -> B.ByteString
dearmor32 s = doit 0 0 s
    where
      doit _ _ [] = B.empty
      doit carryVal carrySize (c1:s) =
          let v = b2a ! c1
          in if v == inval
             then B.empty
             else let needbits = 8 - carrySize
                      nextCarrySize = 5 - needbits
                      b = carryVal .|. (shift (b2a ! c1) (negate nextCarrySize))
                      nextCarry = shift v (8 - nextCarrySize)
                  in if nextCarrySize < 0
                     then doit b (nextCarrySize + 8) s
                     else B.cons b $ doit nextCarry nextCarrySize s


--
-- Labeled storage
--

label2path   :: (Label l) => l -> FilePath
label2path l = show $ sha224 $ L.pack $ show l

init   :: (Label l) => l -> LIO l s ()
init l = rethrowTCB $ ioTCB $ do
         createDirectory "LabeledStorage"
         

--
-- Crap
--

mask n = complement $ shift (fromInteger $ -1) n
pack s = C.pack s

lgetLine :: (Label l, Typeable s) => LIO l s String
lgetLine = ioTCB getLine
lputStr x = ioTCB $ putStr x
lputStrLn x = ioTCB $ putStrLn x

