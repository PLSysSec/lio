{-# LANGUAGE Safe #-}

{- | 'MonadLIO' generalizations for "LIO.Concurrent.LMVar". -}
module LIO.Concurrent.LMVar.Trans
  ( newEmptyLMVar
  , newEmptyLMVarP
  , newLMVar
  , newLMVarP
  , takeLMVar
  , takeLMVarP
  , tryTakeLMVar
  , tryTakeLMVarP
  , putLMVar
  , putLMVarP
  , tryPutLMVar
  , tryPutLMVarP
  , readLMVar
  , readLMVarP
  , swapLMVar
  , swapLMVarP
  , isEmptyLMVar
  , isEmptyLMVarP
  ) where

import safe Control.Concurrent.MVar

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label

import safe qualified LIO.Concurrent.LMVar as M

-- | See 'LIO.Concurrent.LMVar.newEmptyLMVar'.
newEmptyLMVar :: (MonadLIO l m, Label l) => l -> m (M.LMVar l a)
newEmptyLMVar = liftLIO . M.newEmptyLMVar

-- | See 'LIO.Concurrent.LMVar.newEmptyLMVarP'.
newEmptyLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m (M.LMVar l a)
newEmptyLMVarP p = liftLIO . M.newEmptyLMVarP p

-- | See 'LIO.Concurrent.LMVar.newLMVar'.
newLMVar :: (MonadLIO l m, Label l) => l -> a -> m (M.LMVar l a)
newLMVar l = liftLIO . M.newLMVar l

-- | See 'LIO.Concurrent.LMVar.newLMVarP'.
newLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> a -> m (M.LMVar l a)
newLMVarP p l = liftLIO . M.newLMVarP p l

-- | See 'LIO.Concurrent.LMVar.takeLMVar'.
takeLMVar :: (MonadLIO l m, Label l) => M.LMVar l a -> m a
takeLMVar = liftLIO . M.takeLMVar

-- | See 'LIO.Concurrent.LMVar.takeLMVarP'.
takeLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> M.LMVar l a -> m a
takeLMVarP p = liftLIO . M.takeLMVarP p

-- | See 'LIO.Concurrent.LMVar.tryTakeLMVar'.
tryTakeLMVar :: (MonadLIO l m, Label l) => M.LMVar l a -> m (Maybe a)
tryTakeLMVar = liftLIO . M.tryTakeLMVar

-- | See 'LIO.Concurrent.LMVar.tryTakeLMVarP'.
tryTakeLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> M.LMVar l a -> m (Maybe a)
tryTakeLMVarP p = liftLIO . M.tryTakeLMVarP p

-- | See 'LIO.Concurrent.LMVar.putLMVar'.
putLMVar :: (MonadLIO l m, Label l) => M.LMVar l a -> a -> m ()
putLMVar mvar = liftLIO . M.putLMVar mvar

-- | See 'LIO.Concurrent.LMVar.putLMVarP'.
putLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> M.LMVar l a -> a -> m ()
putLMVarP p mvar = liftLIO . M.putLMVarP p mvar

-- | See 'LIO.Concurrent.LMVar.tryPutLMVar'.
tryPutLMVar :: (MonadLIO l m, Label l) => M.LMVar l a -> a -> m Bool
tryPutLMVar mvar = liftLIO . M.tryPutLMVar mvar

-- | See 'LIO.Concurrent.LMVar.tryPutLMVarP'.
tryPutLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> M.LMVar l a -> a -> m Bool
tryPutLMVarP p mvar = liftLIO . M.tryPutLMVarP p mvar

-- | See 'LIO.Concurrent.LMVar.readLMVar'.
readLMVar :: (MonadLIO l m, Label l) => M.LMVar l a -> m a
readLMVar = liftLIO . M.readLMVar

-- | See 'LIO.Concurrent.LMVar.readLMVarP'.
readLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> M.LMVar l a -> m a
readLMVarP p = liftLIO . M.readLMVarP p

-- | See 'LIO.Concurrent.LMVar.swapLMVar'.
swapLMVar :: (MonadLIO l m, Label l) => M.LMVar l a -> a -> m a
swapLMVar mvar = liftLIO . M.swapLMVar mvar

-- | See 'LIO.Concurrent.LMVar.swapLMVarP'.
swapLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> M.LMVar l a -> a -> m a
swapLMVarP p mvar = liftLIO . M.swapLMVarP p mvar

-- | See 'LIO.Concurrent.LMVar.isEmptyLMVar'.
isEmptyLMVar :: (MonadLIO l m, Label l) => M.LMVar l a -> m Bool
isEmptyLMVar = liftLIO . M.isEmptyLMVar

-- | See 'LIO.Concurrent.LMVar.isEmptyLMVarP'.
isEmptyLMVarP :: (MonadLIO l m, PrivDesc l p) => Priv p -> M.LMVar l a -> m Bool
isEmptyLMVarP p = liftLIO . M.isEmptyLMVarP p
