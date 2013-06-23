{-# LANGUAGE Unsafe #-}

{- |

This module exports 'LabeledResult's which are effectively thread exit
results protected by a label. See "LIO.Concurrent" for a description
of the concurrency abstractions of LIO.

-}

module LIO.TCB.Concurrent (
    LabeledResult(..), LResStatus(..)
  ) where

import safe qualified Control.Concurrent as IO
import safe Data.IORef

import LIO.TCB (LabelOf(..))

data LResStatus l a = LResEmpty
                    | LResLabelTooHigh !l
                    | LResResult a
                      deriving (Show)

-- | A LabeledResult encapsulates a future result from a computation
-- spawned by 'lFork' or 'lForkP'.
data LabeledResult l a = LabeledResultTCB {
    lresThreadIdTCB :: !IO.ThreadId
    -- ^ Thread executing the computation
  , lresLabelTCB :: !l
    -- ^ Label of the tresult
  , lresBlockTCB :: !(IO.MVar ())
  , lresStatusTCB :: !(IORef (LResStatus l a))
    -- ^ Result (when it is ready), or the label at which the thread
    -- terminated, if that label could not flow to 'lresLabelTCB'.
  }

instance LabelOf LabeledResult where
  labelOf = lresLabelTCB
