{-# LANGUAGE Trustworthy #-}

{- | 'MonadLIO' generalizations for "LIO.Labeled". -}
module LIO.Labeled.Trans where

import safe Control.Monad

import safe LIO.Error
import safe LIO.Label
import safe LIO.Core
import LIO.TCB

-- | See 'LIO.Labeled.label'.
label :: (MonadLIO l m, Label l) => l -> a -> m (Labeled l a)
label l a = liftLIO $ do
  withContext "label" $ guardAlloc l
  return $ LabeledTCB l a

-- | See 'LIO.Labeled.labelP'.
labelP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> a -> m (Labeled l a)
labelP p l a = liftLIO $ do
  withContext "labelP" $ guardAllocP p l
  return $ LabeledTCB l a

-- | See 'LIO.Labeled.unlabel'.
unlabel :: (MonadLIO l m, Label l) => Labeled l a -> m a
unlabel (LabeledTCB l v) = liftLIO $ withContext "unlabel" (taint l) >> return v

-- | See 'LIO.Labeled.unlabelP'.
unlabelP :: (MonadLIO l m, PrivDesc l p) => Priv p -> Labeled l a -> m a
unlabelP p (LabeledTCB l v) = liftLIO $ withContext "unlabelP" (taintP p l) >> return v

-- | See 'LIO.Labeled.relabelLabeledP'.
relabelLabeledP :: (MonadLIO l m, PrivDesc l p)
                => Priv p -> l -> Labeled l a -> m (Labeled l a)
relabelLabeledP p newl (LabeledTCB oldl v) = liftLIO $ do
  clr <- getClearance
  unless (canFlowTo newl clr     &&
          canFlowToP p newl oldl &&
          canFlowToP p oldl newl) $ labelErrorP "relabelLabeledP" p [oldl, newl]
  return $ LabeledTCB newl v

-- | See 'LIO.Labeled.taintLabeled'.
taintLabeled :: (MonadLIO l m, Label l) => l -> Labeled l a -> m (Labeled l a)
taintLabeled l (LabeledTCB lold v) = liftLIO $ do
  let lnew = lold `lub` l
  withContext "taintLabeled" $ guardAlloc lnew
  return $ LabeledTCB lnew v

-- | See 'LIO.Labeled.taintLabeledP'.
taintLabeledP :: (MonadLIO l m, PrivDesc l p)
              => Priv p -> l -> Labeled l a -> m (Labeled l a)
taintLabeledP p l (LabeledTCB lold v) = liftLIO $ do
  let lnew = lold `lub` l
  withContext "taintLabeledP" $ guardAllocP p lnew
  return $ LabeledTCB lnew v

-- | See 'LIO.Labeled.lFmap'.
lFmap :: (MonadLIO l m, Label l) => Labeled l a -> (a -> b) -> m (Labeled l b)
lFmap (LabeledTCB lold v) f = liftLIO $ do
  l <- getLabel
  -- Result label is joined with current label
  let lnew = lold `lub` l
  -- `label` checks for clearance violation then labels
  withContext "lFmap" $ label lnew $ f v

-- | See 'LIO.Labeled.lAp'.
lAp :: (MonadLIO l m, Label l) => Labeled l (a -> b) -> Labeled l a -> m (Labeled l b)
lAp (LabeledTCB lf f) (LabeledTCB la a) = liftLIO $ do
  l <- getLabel
  let lnew = l `lub` lf `lub` la
  withContext "lAp" $ label lnew $ f a
