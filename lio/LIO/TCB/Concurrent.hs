{-# LANGUAGE Unsafe #-}

{- |

This module exports 'LabeledResult's which are effectively thread exit
results protected by a label. See "LIO.Concurrent" for a description
of the concurrency abstractions of LIO.

-}

module LIO.TCB.Concurrent (
    LabeledResult(..), LResStatus(..)
  ) where

import qualified Control.Concurrent as IO
import Data.IORef

import LIO.Label

data LResStatus l a = LResEmpty
                    | LResLabelTooHigh !l
                    | LResResult a
                      deriving (Show)

-- | A LabeledResult encapsulates a future result from a computation running
-- in a thread. It holds the 'ThreadId' and an 'LMVar' where the result is
-- stored. The thread referenced in 'lresThreadIdTCB' should fill in
-- 'lresResultTCB' (either with a value or exception), so waiting on the thread
-- should ensure that a result is ready.
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
