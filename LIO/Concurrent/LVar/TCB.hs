-- | Implementatoin of labeled @MVar@s.
module LIO.Concurrent.LVar.TCB ( LVar
                               , newEmptyLVar, newEmptyLVarP
                               , newLVar, newLVarP
                               , takeLVar, takeLVarP
                               , putLVar, putLVarP
                               , readLVar, readLVarP
                               -- TCB
                               , newEmptyLVarTCB
                               , newLVarTCB
                               , takeLVarTCB
                               , putLVarTCB
                               , readLVarTCB
                               ) where

import Control.Concurrent.MVar
import LIO.TCB

-- | @MVar@ with associated label.
data LVar l a = LVarTCB l (MVar a)

newEmptyLVarP :: (Priv l p) => p -> l -> LIO l s (LVar l a)
newEmptyLVarP p l = do
  aguardP p l
  iom <- ioTCB $ newEmptyMVar
  return $ LVarTCB l iom

-- | Create a new empty labeled @MVar@
newEmptyLVar :: (Label l) => l -> LIO l s (LVar l a)
newEmptyLVar = newEmptyLVarP NoPrivs

newEmptyLVarTCB :: (Label l) => l -> LIO l s (LVar l a)
newEmptyLVarTCB l = do
  iom <- ioTCB $ newEmptyMVar
  return $ LVarTCB l iom

newLVarP :: (Priv l p) => p -> l -> a -> LIO l s (LVar l a)
newLVarP p l a = do
  aguardP p l
  m <- ioTCB $ newMVar a
  return $ LVarTCB l m

-- | Create a new labeled @MVar@
newLVar :: (Label l) => l -> a -> LIO l s (LVar l a)
newLVar = newLVarP NoPrivs

newLVarTCB :: (Label l) => l -> a -> LIO l s (LVar l a)
newLVarTCB l a = do
  m <- ioTCB $ newMVar a
  return $ LVarTCB l m

takeLVarP :: (Priv l p) => p -> LVar l a -> LIO l s a
takeLVarP p (LVarTCB l m) = do
  wguardP p l
  val <- ioTCB $ takeMVar m
  return val

-- | Take an 'LVar'. Note that a take consists of a read and a write.
-- and so the current label must be the same as that of the 'LVar' (of
-- course, this is not the case when using privileges).
takeLVar :: (Label l) => LVar l a -> LIO l s a
takeLVar = takeLVarP NoPrivs

takeLVarTCB :: (Label l) => LVar l a -> LIO l s a
takeLVarTCB (LVarTCB _ m) = do
  val <- ioTCB $ takeMVar m
  return val

putLVarP :: (Priv l p) => p -> LVar l a -> a -> LIO l s ()
putLVarP p (LVarTCB l m) a = do
  wguardP p l
  val <- ioTCB $ putMVar m a
  return val

-- | Put an 'LVar'. Note that a put consists of a read and a write.
-- and so the current label must be the same as that of the 'LVar'
-- (of course, this is not the case when using privileges).
putLVar :: (Label l) => LVar l a -> a -> LIO l s ()
putLVar = putLVarP NoPrivs

putLVarTCB :: (Label l) => LVar l a -> a -> LIO l s ()
putLVarTCB (LVarTCB _ m) a = do
  val <- ioTCB $ putMVar m a
  return val

readLVarP :: (Priv l p) => p -> LVar l a -> LIO l s a
readLVarP p (LVarTCB l m) = do
  wguardP p l
  ioTCB $ readMVar m 

readLVarTCB :: (Label l) => LVar l a -> LIO l s a
readLVarTCB (LVarTCB _ m) = do
  ioTCB $ readMVar m

-- | Combination of 'takeLVar' and 'putLVar'. Read the value, and just
-- put it back.
readLVar :: (Label l) => LVar l a -> LIO l s a
readLVar = readLVarP NoPrivs
