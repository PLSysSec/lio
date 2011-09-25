{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE SafeImports #-}
#else
#warning "This module is not using SafeHaskell"
#endif
-- |This module implements labeled IORefs.  The interface is analogous
-- to "Data.IORef", but the operations take place in the LIO monad.
-- Moreover, reading the LIORef calls taint, while writing it calls
-- wguard.
module LIO.LIORef.TCB (LIORef
                      , newLIORef, labelOfLIORef
                      , readLIORef, writeLIORef, atomicModifyLIORef
                      -- * With privileges
                      , newLIORefP
                      , readLIORefP, writeLIORefP, atomicModifyLIORefP
                      -- * TCB
                      , newLIORefTCB
                      , readLIORefTCB, writeLIORefTCB, atomicModifyLIORefTCB
                      ) where

import LIO.TCB

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
import safe Data.IORef
#else
import Data.IORef
#endif


data LIORef l a = LIORefTCB l (IORef a)


newLIORefP :: (Priv l p) => p -> l -> a -> LIO l s (LIORef l a)
newLIORefP p l a = do
  aguardP p l
  ior <- ioTCB $ newIORef a
  return $ LIORefTCB l ior

newLIORef :: (Label l) => l -> a -> LIO l s (LIORef l a)
newLIORef = newLIORefP NoPrivs

newLIORefTCB :: (Label l) => l -> a -> LIO l s (LIORef l a)
newLIORefTCB l a = do
  ior <- ioTCB $ newIORef a
  return $ LIORefTCB l ior

--

labelOfLIORef :: (Label l) => LIORef l a -> l
labelOfLIORef (LIORefTCB l _) = l

--

readLIORefP :: (Priv l p) => p -> LIORef l a -> LIO l s a
readLIORefP p (LIORefTCB l r) = do
  taintP p l
  val <- ioTCB $ readIORef r
  return val

readLIORef :: (Label l) => LIORef l a -> LIO l s a
readLIORef = readLIORefP NoPrivs

readLIORefTCB :: (Label l) => LIORef l a -> LIO l s a
readLIORefTCB (LIORefTCB _ r) = ioTCB $ readIORef r

--

writeLIORefP :: (Priv l p) => p -> LIORef l a -> a -> LIO l s ()
writeLIORefP p (LIORefTCB l r) a = do
  aguardP p l
  ioTCB $ writeIORef r a

writeLIORef :: (Label l) => LIORef l a -> a -> LIO l s ()
writeLIORef = writeLIORefP NoPrivs 

writeLIORefTCB :: (Label l) => LIORef l a -> a -> LIO l s ()
writeLIORefTCB (LIORefTCB _ r) a = ioTCB $ writeIORef r a

--

atomicModifyLIORefP :: (Priv l p) =>
                       p -> LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORefP p (LIORefTCB l r) f = do
  aguardP p l
  ioTCB $ atomicModifyIORef r f

atomicModifyLIORef :: (Label l) =>
                      LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORef = atomicModifyLIORefP NoPrivs

atomicModifyLIORefTCB :: (Label l) =>
                      LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORefTCB (LIORefTCB _ r) f = ioTCB $ atomicModifyIORef r f

