{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 'MonadLIO' generalizations for "LIO.Core". -}
module LIO.Core.Trans where

import safe Control.Monad

import safe LIO.Error
import safe LIO.Label
import safe LIO.Monad
import safe LIO.Run
import LIO.TCB

-- | See 'LIO.Core.getLabel'.
getLabel :: (MonadLIO l m, Label l) => m l
getLabel = lioLabel `liftM` liftLIO getLIOStateTCB

-- | See 'LIO.Core.setLabel'.
setLabel :: (MonadLIO l m, Label l) => l -> m ()
setLabel l = liftLIO . withContext "setLabel" $ do
  guardAlloc l
  modifyLIOStateTCB $ \s -> s { lioLabel = l }

-- | See 'LIO.Core.setLabelP'.
setLabelP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
setLabelP p l = liftLIO . withContext "setLabelP" $ do
  guardAllocP p l
  modifyLIOStateTCB $ \s -> s { lioLabel = l }

-- | See 'LIO.Core.getClearance'.
getClearance :: (MonadLIO l m, Label l) => m l
getClearance = lioClearance `liftM` liftLIO getLIOStateTCB

-- | See 'LIO.Core.setClearance'.
setClearance :: (MonadLIO l m, Label l) => l -> m ()
setClearance cnew = liftLIO $ do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowTo l cnew && canFlowTo cnew c) $
    labelError "setClearance" [cnew]
  putLIOStateTCB $ LIOState l cnew

-- | See 'LIO.Core.setClearanceP'.
setClearanceP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
setClearanceP p cnew = liftLIO $ do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowTo l cnew && canFlowToP p cnew c) $
    labelErrorP "setClearanceP" p [cnew]
  putLIOStateTCB $ LIOState l cnew

-- | See 'LIO.Core.guardAlloc'.
guardAlloc :: (MonadLIO l m, Label l) => l -> m ()
guardAlloc newl = liftLIO $ do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowTo l newl && canFlowTo newl c) $
    labelError "guardAllocP" [newl]

-- | See 'LIO.Core.guardAllocP'.
guardAllocP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
guardAllocP p newl = liftLIO $ do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowToP p l newl && canFlowTo newl c) $
    labelErrorP "guardAllocP" p [newl]

-- | See 'LIO.Core.taint'.
taint :: (MonadLIO l m, Label l) => l -> m ()
taint newl = liftLIO $ do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  let l' = l `lub` newl
  unless (l' `canFlowTo` c) $ labelError "taint" [newl]
  modifyLIOStateTCB $ \s -> s { lioLabel = l' }

-- | See 'LIO.Core.taintP'.
taintP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
taintP p newl = liftLIO $ do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  let l' = l `lub` downgradeP p newl
  unless (l' `canFlowTo` c) $ labelErrorP "taintP" p [newl]
  modifyLIOStateTCB $ \s -> s { lioLabel = l' }

-- | See 'LIO.Core.guardWrite'.
guardWrite :: (MonadLIO l m, Label l) => l -> m ()
guardWrite newl = liftLIO . withContext "guardWrite" $ do
  guardAlloc newl
  taint newl

-- | See 'LIO.Core.guardWriteP'.
guardWriteP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m ()
guardWriteP p newl = liftLIO . withContext "guardWriteP" $ do
  guardAllocP p newl
  taintP p newl
