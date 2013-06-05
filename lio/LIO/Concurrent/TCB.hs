{-# LANGUAGE Unsafe #-}

{- |

This module exports 'LabeledResult's which are effectively thread exit
results protected by a label. See "LIO.Concurrent" for a description
of the concurrency abstractions of LIO.

-}

module LIO.Concurrent.TCB (
    LabeledResult(..), forkLIOTCB
  , threadDelay
  ) where

import Control.Monad
import LIO.Label
import LIO.Core
import LIO.Concurrent.LMVar
import LIO.TCB
import Control.Concurrent hiding (threadDelay)
import qualified Control.Concurrent as C

-- | Execute an 'LIO' computation in a new lightweight thread. The
-- 'ThreadId' of the newly created thread is returned.
forkLIOTCB :: Label l => LIO l () -> LIO l ThreadId
forkLIOTCB act = do
  s <- getLIOStateTCB
  ioTCB . forkIO . void $ tryLIO act s

-- | A LabeledResult encapsulates a future result from a computation running
-- in a thread. It holds the 'ThreadId' and an 'LMVar' where the result is
-- stored. The thread referenced in 'lresThreadIdTCB' should fill in
-- 'lresResultTCB' (either with a value or exception), so waiting on the thread
-- should ensure that a result is ready.
data LabeledResult l a = LabeledResultTCB {
    lresThreadIdTCB :: ThreadId
    -- ^ Thread executing the computation
  , lresResultTCB :: LMVar l (Either (LabeledException l) a) 
    -- ^ Plecement of computation result
  }

instance LabelOf LabeledResult where
  labelOf = labelOf . lresResultTCB

-- | Suspend current thread for a given number of microseconds.
threadDelay :: Label l => Int -> LIO l ()
threadDelay = ioTCB . C.threadDelay

