{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

{- | 'MonadLIO' generalizations for "LIO.LIORef". -}
module LIO.LIORef.Trans where

import safe Data.IORef

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label
import LIO.TCB.LObj

import safe LIO.LIORef

-- | See 'LIO.LIORef.newLIORef'.
newLIORef :: (MonadLIO l m, Label l)
          => l                  -- ^ Label of reference
          -> a                  -- ^ Initial value
          -> m (LIORef l a) -- ^ Mutable reference
newLIORef l a = liftLIO . guardIOTCB (withContext "newLIORef" $ guardAlloc l) $
  LObjTCB l `fmap` newIORef a

-- | See 'LIO.LIORef.newLIORefP'.
newLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> a -> m (LIORef l a)
newLIORefP p l a = liftLIO . guardIOTCB (withContext "newLIORefP" $ guardAllocP p l) $
  LObjTCB l `fmap` newIORef a

-- | See 'LIO.LIORef.readLIORef'.
readLIORef :: (MonadLIO l m, Label l) => LIORef l a -> m a
readLIORef = liftLIO . blessReadOnlyTCB "readLIORef" readIORef

-- | See 'LIO.LIORef.readLIORefP'.
readLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LIORef l a -> m a
readLIORefP priv = liftLIO . blessReadOnlyPTCB "readLIORefP" readIORef priv

-- | See 'LIO.LIORef.writeLIORef'.
writeLIORef :: (MonadLIO l m, Label l) => LIORef l a -> a -> m ()
writeLIORef ref = liftLIO . blessWriteOnlyTCB "writeLIORef" writeIORef ref

-- | See 'LIO.LIORef.writeLIORefP'.
writeLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LIORef l a -> a -> m ()
writeLIORefP priv ref = liftLIO . blessWriteOnlyPTCB "writeLIORefP" writeIORef priv ref

-- | See 'LIO.LIORef.modifyLIORef'.
modifyLIORef :: (MonadLIO l m, Label l)
             => LIORef l a             -- ^ Labeled reference
             -> (a -> a)               -- ^ Modifier
             -> m ()
modifyLIORef ref = liftLIO . blessWriteOnlyTCB "modifyLIORef" modifyIORef ref

-- | See 'LIO.LIORef.modifyLIORefP'.
modifyLIORefP :: (MonadLIO l m, PrivDesc l p)
              =>  Priv p -> LIORef l a -> (a -> a) -> m ()
modifyLIORefP priv ref = liftLIO . blessWriteOnlyPTCB "modifyLIORefP" modifyIORef priv ref

-- | See 'LIO.LIORef.atomicModifyLIORef'.
atomicModifyLIORef :: (MonadLIO l m, Label l) => LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORef ref = liftLIO . blessTCB "atomicModifyIORef" atomicModifyIORef ref

-- | See 'LIO.LIORef.atomicModifyLIORefP'.
atomicModifyLIORefP :: (MonadLIO l m, PrivDesc l p)
                    => Priv p -> LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORefP priv ref = liftLIO . blessPTCB "atomicModifyLIORefP" atomicModifyIORef priv ref
