{-# LANGUAGE Trustworthy #-}

{- | 'MonadLIO' generalizations for "LIO.Labeled". -}
module LIO.Labeled.Trans (
    label
  , labelP
  , unlabel
  , unlabelP
  , relabelLabeledP
  , taintLabeled
  , taintLabeledP
  , lFmap
  , lAp
  ) where

import safe LIO.Label
import safe LIO.Core
import LIO.TCB

import safe qualified LIO.Labeled as L

-- | See 'LIO.Labeled.label'.
label :: (MonadLIO l m, Label l) => l -> a -> m (Labeled l a)
label l = liftLIO . L.label l

-- | See 'LIO.Labeled.labelP'.
labelP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> a -> m (Labeled l a)
labelP p l = liftLIO . L.labelP p l

-- | See 'LIO.Labeled.unlabel'.
unlabel :: (MonadLIO l m, Label l) => Labeled l a -> m a
unlabel = liftLIO . L.unlabel

-- | See 'LIO.Labeled.unlabelP'.
unlabelP :: (MonadLIO l m, PrivDesc l p) => Priv p -> Labeled l a -> m a
unlabelP p = liftLIO . L.unlabelP p

-- | See 'LIO.Labeled.relabelLabeledP'.
relabelLabeledP :: (MonadLIO l m, PrivDesc l p)
                => Priv p -> l -> Labeled l a -> m (Labeled l a)
relabelLabeledP p newl = liftLIO . L.relabelLabeledP p newl

-- | See 'LIO.Labeled.taintLabeled'.
taintLabeled :: (MonadLIO l m, Label l) => l -> Labeled l a -> m (Labeled l a)
taintLabeled l = liftLIO . L.taintLabeled l

-- | See 'LIO.Labeled.taintLabeledP'.
taintLabeledP :: (MonadLIO l m, PrivDesc l p)
              => Priv p -> l -> Labeled l a -> m (Labeled l a)
taintLabeledP p l = liftLIO . L.taintLabeledP p l

-- | See 'LIO.Labeled.lFmap'.
lFmap :: (MonadLIO l m, Label l) => Labeled l a -> (a -> b) -> m (Labeled l b)
lFmap l = liftLIO . L.lFmap l

-- | See 'LIO.Labeled.lAp'.
lAp :: (MonadLIO l m, Label l) => Labeled l (a -> b) -> Labeled l a -> m (Labeled l b)
lAp lf = liftLIO . L.lAp lf
