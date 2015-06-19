{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{- | 'MonadLIO' generalizations for "LIO.Error". -}
module LIO.Error.Trans where

import safe LIO.Exception
import safe LIO.Label
import safe LIO.Monad
import LIO.TCB

import safe LIO.Error

-- | See 'LIO.Error.labelError'.
labelError :: (MonadLIO l m, Label l)
    => String -- ^ Function that failed.
    -> [l]    -- ^ Labels involved in error.
    -> m a
labelError fl ls = liftLIO $ do
  st <- getLIOStateTCB
  throwLIO LabelError {
      lerrContext = []
    , lerrFailure = fl
    , lerrCurLabel = lioLabel st
    , lerrCurClearance = lioClearance st
    , lerrPrivs = []
    , lerrLabels = ls
    }

-- | See 'LIO.Error.labelErrorP'.
labelErrorP :: (MonadLIO l m, Label l, PrivDesc l p)
    => String  -- ^ Function that failed.
    -> Priv p  -- ^ Privileges involved.
    -> [l]     -- ^ Labels involved.
    -> m a
labelErrorP fl p ls = liftLIO $ do
  st <- getLIOStateTCB
  throwLIO LabelError {
      lerrContext = []
    , lerrFailure = fl
    , lerrCurLabel = lioLabel st
    , lerrCurClearance = lioClearance st
    , lerrPrivs = [GenericPrivDesc $ privDesc p]
    , lerrLabels = ls
    }
