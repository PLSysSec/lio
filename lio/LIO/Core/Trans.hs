{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 'MonadLIO' generalizations for "LIO.Core". -}
module LIO.Core.Trans (
    getLabel
  , setLabel
  , setLabelP
  , getClearance
  , setClearance
  , setClearanceP
  , guardAlloc
  , guardAllocP
  , taint
  , taintP
  , guardWrite
  , guardWriteP
  ) where

import safe LIO.Label
import safe LIO.Monad

import safe qualified LIO.Core as C

-- | See 'LIO.Core.getLabel'.
getLabel :: (MonadLIO l m, Label l) => m l
getLabel = liftLIO C.getLabel

-- | See 'LIO.Core.setLabel'.
setLabel :: (MonadLIO l m, Label l) => l -> m ()
setLabel = liftLIO . C.setLabel

-- | See 'LIO.Core.setLabelP'.
setLabelP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
setLabelP p = liftLIO . C.setLabelP p

-- | See 'LIO.Core.getClearance'.
getClearance :: (MonadLIO l m, Label l) => m l
getClearance = liftLIO C.getClearance

-- | See 'LIO.Core.setClearance'.
setClearance :: (MonadLIO l m, Label l) => l -> m ()
setClearance = liftLIO . C.setClearance

-- | See 'LIO.Core.setClearanceP'.
setClearanceP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
setClearanceP p = liftLIO . C.setClearanceP p

-- | See 'LIO.Core.guardAlloc'.
guardAlloc :: (MonadLIO l m, Label l) => l -> m ()
guardAlloc = liftLIO . C.guardAlloc

-- | See 'LIO.Core.guardAllocP'.
guardAllocP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
guardAllocP p = liftLIO . C.guardAllocP p

-- | See 'LIO.Core.taint'.
taint :: (MonadLIO l m, Label l) => l -> m ()
taint = liftLIO . C.taint

-- | See 'LIO.Core.taintP'.
taintP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
taintP p = liftLIO . C.taintP p

-- | See 'LIO.Core.guardWrite'.
guardWrite :: (MonadLIO l m, Label l) => l -> m ()
guardWrite = liftLIO . C.guardWrite

-- | See 'LIO.Core.guardWriteP'.
guardWriteP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
guardWriteP p = liftLIO . C.guardWriteP p
