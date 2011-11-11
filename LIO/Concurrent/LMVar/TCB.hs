{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE SafeImports #-}
#else
#warning "This module is not using SafeHaskell"
#endif
-- | Implementatoin of labeled @MVar@s.
module LIO.Concurrent.LMVar.TCB ( LMVar
                               , newEmptyLMVar, newEmptyLMVarP
                               , newLMVar, newLMVarP
                               , takeLMVar, takeLMVarP
                               , putLMVar, putLMVarP
                               , readLMVar, readLMVarP
                               -- TCB
                               , newEmptyLMVarTCB
                               , newLMVarTCB
                               , takeLMVarTCB
                               , putLMVarTCB
                               , readLMVarTCB
                               ) where

import Control.Concurrent.MVar
import LIO.TCB

-- | @MVar@ with associated label.
data LMVar l a = LMVarTCB l (MVar a)

newEmptyLMVarP :: (Priv l p) => p -> l -> LIO l s (LMVar l a)
newEmptyLMVarP p l = do
  aguardP p l
  iom <- ioTCB $ newEmptyMVar
  return $ LMVarTCB l iom

-- | Create a new empty labeled @MVar@
newEmptyLMVar :: (Label l) => l -> LIO l s (LMVar l a)
newEmptyLMVar = newEmptyLMVarP NoPrivs

newEmptyLMVarTCB :: (Label l) => l -> LIO l s (LMVar l a)
newEmptyLMVarTCB l = do
  iom <- ioTCB $ newEmptyMVar
  return $ LMVarTCB l iom

newLMVarP :: (Priv l p) => p -> l -> a -> LIO l s (LMVar l a)
newLMVarP p l a = do
  aguardP p l
  m <- ioTCB $ newMVar a
  return $ LMVarTCB l m

-- | Create a new labeled @MVar@
newLMVar :: (Label l) => l -> a -> LIO l s (LMVar l a)
newLMVar = newLMVarP NoPrivs

newLMVarTCB :: (Label l) => l -> a -> LIO l s (LMVar l a)
newLMVarTCB l a = do
  m <- ioTCB $ newMVar a
  return $ LMVarTCB l m

takeLMVarP :: (Priv l p) => p -> LMVar l a -> LIO l s a
takeLMVarP p (LMVarTCB l m) = do
  wguardP p l
  val <- ioTCB $ takeMVar m
  return val

-- | Take an 'LMVar'. Note that a take consists of a read and a write.
-- and so the current label must be the same as that of the 'LMVar' (of
-- course, this is not the case when using privileges).
takeLMVar :: (Label l) => LMVar l a -> LIO l s a
takeLMVar = takeLMVarP NoPrivs

takeLMVarTCB :: (Label l) => LMVar l a -> LIO l s a
takeLMVarTCB (LMVarTCB _ m) = do
  val <- ioTCB $ takeMVar m
  return val

putLMVarP :: (Priv l p) => p -> LMVar l a -> a -> LIO l s ()
putLMVarP p (LMVarTCB l m) a = do
  wguardP p l
  val <- ioTCB $ putMVar m a
  return val

-- | Put an 'LMVar'. Note that a put consists of a read and a write.
-- and so the current label must be the same as that of the 'LMVar'
-- (of course, this is not the case when using privileges).
putLMVar :: (Label l) => LMVar l a -> a -> LIO l s ()
putLMVar = putLMVarP NoPrivs

putLMVarTCB :: (Label l) => LMVar l a -> a -> LIO l s ()
putLMVarTCB (LMVarTCB _ m) a = do
  val <- ioTCB $ putMVar m a
  return val

readLMVarP :: (Priv l p) => p -> LMVar l a -> LIO l s a
readLMVarP p (LMVarTCB l m) = do
  wguardP p l
  ioTCB $ readMVar m 

readLMVarTCB :: (Label l) => LMVar l a -> LIO l s a
readLMVarTCB (LMVarTCB _ m) = do
  ioTCB $ readMVar m

-- | Combination of 'takeLMVar' and 'putLMVar'. Read the value, and just
-- put it back.
readLMVar :: (Label l) => LMVar l a -> LIO l s a
readLMVar = readLMVarP NoPrivs
