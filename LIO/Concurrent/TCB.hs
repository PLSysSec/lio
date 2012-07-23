{-# LANGUAGE Unsafe #-}

{- |

This module exports labeled results which are used by the safe
functions of "LIO.Concurrent".

-}

module LIO.Concurrent.TCB (
    LabeledResult(..), ThreadId
  ) where

import LIO.Label
import LIO.Core
import LIO.Concurrent.LMVar
import Control.Concurrent

-- | A labeled thread result is simply a wrapper for a labeled MVar. A
-- thread can observe the result of another thread, only after raising
-- its label to the label of the result.
data LabeledResult l a = LabeledResultTCB {
    lresThreadIdTCB :: ThreadId 
    -- ^ Thread executing the computation
  , lresResultTCB :: LMVar l (Either (LabeledException l) a) 
    -- ^ Plecement of computation result
  }

instance LabelOf LabeledResult where
  labelOf = labelOf . lresResultTCB
