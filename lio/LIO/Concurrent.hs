{-# LANGUAGE Trustworthy #-}
{- |

This module exposes useful concurrency abstrations for 'LIO'. This
module is, in part, analogous to "Control.Concurrent". Specifically,
LIO provides a means for spawning 'LIO' computations in a new thread
with 'forkLIO'.  LIO relies on the lightweight threads managed by
Haskell's runtime system; we do not provide a way to fork OS-level
threads.

In addition to this, LIO also provides 'lFork' and 'lWait' which allow
forking of a computation that is restricted from reading data more
sensitive than a given upper bound. This limit is different from
clearance in that it allows the computation to spawn additional
threads with an upper bound above said upper bound label, but below
the clearance. The 'lFork' function should be used whenever an LIO
computation wishes to execute a sub-computation that may raise the
current label (up to the supplied upper bound).  To this end, the
current label only needs to be raised when the computation is
interested in reading the result of the sub-computation. The role of
'lWait' is precisely this: raise the current label and return the
result of such a sub-computation.

-}
module LIO.Concurrent (
    LabeledResult
  -- * Forking new threads
  , forkLIO, lForkP, lFork
  -- * Waiting on threads
  , lWaitP, lWait
  , trylWaitP, trylWait
  , timedlWaitP, timedlWait
  ) where


import Control.Monad

import LIO.Concurrent.TCB
import LIO.Core
import LIO.Label
import LIO.Privs
import LIO.TCB


--
-- Fork
--

-- | Execute an 'LIO' computation in a new lightweight thread.
forkLIO :: LIO l () -> LIO l ()
forkLIO lio = do
  s <- getLIOState
  ioTCB $ void $ runLIO lio s

-- | Labeled fork. @lFork@ allows one to invoke computations that
-- would otherwise raise the current label, but without actually
-- raising the label. The computation is executed in a separate thread
-- and writes its result into a labeled result (whose label is
-- supplied). To observe the result of the computation, or if the
-- computation has terminated, one will have to call 'lWait' and
-- raise the current label. Of couse,  this can be postponed until the
-- result is needed.
--
-- @lFork@ takes a label, which corresponds to the label of the
-- result.  It is required that this label be between the current
-- label and clearance as enforced by a call to 'guardAlloc'.
-- Moreover, the supplied computation must not terminate with its
-- label above the result label; doing so will result in an exception
-- (whose label will reflect this observation) being thrown in the
-- thread reading the result.
--
-- If an exception is thrown in the inner computation, the exception
-- label will be raised to the join of the result label and original
-- exception label.
-- 
-- Note that @lFork@ immediately returns a 'LabeledResult', which is
-- essentially a \"future\", or \"promise\". This prevents
-- timing/termination attacks in which the duration of the forked
-- computation affects the duration of the @lFork@.
-- 
-- To guarantee that the computation has completed, it is important that
-- some thread actually touch the future, i.e., perform an 'lWait'.
lFork :: Label l
      => l                -- ^ Label of result
      -> LIO l a          -- ^ Computation to execute in separate thread
      -> LIO l (LabeledResult l a) -- ^ Labeled result
lFork = lForkP noPrivs


--
-- Wait
--

-- | Given a 'LabeledResult' (a future), @lWait@ returns the unwrapped
-- result (blocks, if necessary). For @lWait@ to succeed, the label of
-- the result must be above the current label and below the current
-- clearance. Moreover, before block-reading, @lWait@ raises the current
-- label to the join of the current label and label of result.  An
-- exception is thrown by the underlying 'guardWrite' if this is not the
-- case.  Additionally, if the thread terminates with an exception (for
-- example if it violates clearance), the exception is rethrown by
-- @lWait@. Similarly, if the thread reads values above the result label,
-- an exception is thrown in place of the result.
lWait :: Label l => LabeledResult l a -> LIO l a
lWait = lWaitP noPrivs

-- | Same as 'lWait', but does not block waiting for result.
trylWait :: Label l => LabeledResult l a -> LIO l (Maybe a)
trylWait = trylWaitP noPrivs

-- | Like 'lWait', with two differences.  First, a timeout is
-- specified and the thread is unconditionally killed after this
-- timeout (if it has not yet returned a value).  Second, if the
-- thread's result exceeds its label @timedWait@ and exceeds what the
-- calling thread can observe, consumes the whole timeout and throws a
-- 'ResultExceedsLabel' exception you can catch (i.e., it never raises
-- the label above the clearance).
timedlWait :: Label l => LabeledResult l a -> Int -> LIO l a
timedlWait = timedlWaitP noPrivs
