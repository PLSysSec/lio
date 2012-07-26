{-# LANGUAGE Unsafe #-}

{- |

This module exports 'LabeledResult's which are effectively thread exit
results protected by a label. See "LIO.Concurrent" for a description
of the concurrency abstractions of LIO.

-}

module LIO.Concurrent.TCB (
    LabeledResult(..), ThreadId
  ) where

import LIO.Label
import LIO.Core
import LIO.Concurrent.LMVar
import Control.Concurrent

-- | A labeled thread result is simply a wrapper for a 'LMVar'. A thread
-- can observe the result of another thread, only after raising its label
-- to the label of the result.
data LabeledResult l a = LabeledResultTCB {
    lresThreadIdTCB :: ThreadId 
    -- ^ Thread executing the computation
  , lresResultTCB :: LMVar l (Either (LabeledException l) a) 
    -- ^ Plecement of computation result
  }

instance LabelOf LabeledResult where
  labelOf = labelOf . lresResultTCB
