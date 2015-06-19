{-# LANGUAGE Safe #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

{- | 'MonadLIO' generalizations for "LIO.LIORef". -}
module LIO.LIORef.Trans (
    newLIORef
  , newLIORefP
  , readLIORef
  , readLIORefP
  , writeLIORef
  , writeLIORefP
  , modifyLIORef
  , modifyLIORefP
  , atomicModifyLIORef
  , atomicModifyLIORefP
  ) where

import safe LIO.Core
import safe LIO.Label

import safe LIO.LIORef (LIORef)
import safe qualified LIO.LIORef as R

-- | See 'LIO.LIORef.newLIORef'.
newLIORef :: (MonadLIO l m, Label l)
          => l                  -- ^ Label of reference
          -> a                  -- ^ Initial value
          -> m (LIORef l a) -- ^ Mutable reference
newLIORef l = liftLIO . R.newLIORef l

-- | See 'LIO.LIORef.newLIORefP'.
newLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> a -> m (LIORef l a)
newLIORefP p l = liftLIO . R.newLIORefP p l

-- | See 'LIO.LIORef.readLIORef'.
readLIORef :: (MonadLIO l m, Label l) => LIORef l a -> m a
readLIORef = liftLIO . R.readLIORef

-- | See 'LIO.LIORef.readLIORefP'.
readLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LIORef l a -> m a
readLIORefP priv = liftLIO . R.readLIORefP priv

-- | See 'LIO.LIORef.writeLIORef'.
writeLIORef :: (MonadLIO l m, Label l) => LIORef l a -> a -> m ()
writeLIORef ref = liftLIO . R.writeLIORef ref

-- | See 'LIO.LIORef.writeLIORefP'.
writeLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LIORef l a -> a -> m ()
writeLIORefP priv ref = liftLIO . R.writeLIORefP priv ref

-- | See 'LIO.LIORef.modifyLIORef'.
modifyLIORef :: (MonadLIO l m, Label l)
             => LIORef l a             -- ^ Labeled reference
             -> (a -> a)               -- ^ Modifier
             -> m ()
modifyLIORef ref = liftLIO . R.modifyLIORef ref

-- | See 'LIO.LIORef.modifyLIORefP'.
modifyLIORefP :: (MonadLIO l m, PrivDesc l p)
              =>  Priv p -> LIORef l a -> (a -> a) -> m ()
modifyLIORefP priv ref = liftLIO . R.modifyLIORefP priv ref

-- | See 'LIO.LIORef.atomicModifyLIORef'.
atomicModifyLIORef :: (MonadLIO l m, Label l) => LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORef ref = liftLIO . R.atomicModifyLIORef ref

-- | See 'LIO.LIORef.atomicModifyLIORefP'.
atomicModifyLIORefP :: (MonadLIO l m, PrivDesc l p)
                    => Priv p -> LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORefP priv ref = liftLIO . R.atomicModifyLIORefP priv ref
