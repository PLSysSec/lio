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
  , ThreadId
  -- * Forking new threads
  , forkLIO, lForkP, lFork
  -- * Waiting on threads
  , lWaitP, lWait
  , trylWaitP, trylWait
  , threadDelay
  -- * Forcing computations (EXPERIMENTAL)
  , AsyncException(..)
  , lBracket, lBracketP
  ) where


import           Control.Monad
import           Control.Concurrent hiding ( threadDelay, ThreadId )
import qualified Control.Concurrent as C
import           Control.Exception ( toException
                                   , Exception
                                   , AsyncException(..))
                 
import           LIO.Label
import           LIO.Core
import           LIO.Labeled
import           LIO.Labeled.TCB
import           LIO.Privs
import           LIO.TCB
                 
import           LIO.Concurrent.TCB
import           LIO.Concurrent.LMVar
import           LIO.Concurrent.LMVar.TCB (tryTakeLMVarTCB, putLMVarTCB)


--
-- Fork
--

-- | Execute an 'LIO' computation in a new lightweight thread. The
-- 'ThreadId' of the newly created thread is returned.
forkLIO :: Label l => LIO l () -> LIO l ThreadId
forkLIO act = do
  s <- getLIOStateTCB
  ioTCB . fmap ThreadIdTCB . forkIO . void $ tryLIO act s


-- | Labeled fork. @lFork@ allows one to invoke computations that
-- would otherwise raise the current label, but without actually
-- raising the label. The computation is executed in a separate thread
-- and writes its result into a labeled result (whose label is
-- supplied). To observe the result of the computation, or if the
-- computation has terminated, one will have to call 'lWait' and
-- raise the current label. Of couse,  this can be postponed until the
-- result is needed.
--
-- @lFork@ takes a label, which corresponds to the label of the result.
-- It is require that this label is above the current label, and below
-- the current clearance as enforced by the underlying 'guardAlloc'.
-- Moreover, the supplied computation must not read anything more
-- sensitive (i.e., with a label above the supplied label) ---  doing so
-- will result in an exception (whose label will reflect this
-- observation) being thrown. 
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

-- | Same as 'lFork', but the supplied set of priviliges are accounted
-- for when performing label comparisons.
lForkP :: PrivDesc l p
       => Priv p -> l -> LIO l a -> LIO l (LabeledResult l a)
lForkP p l act = do
  -- Upperbound is between current label and clearance, asserted by
  -- 'newEmptyLMVarP', otherwise add: guardAllocP p l
  mv <- newEmptyLMVarP p l
  (ThreadIdTCB tid) <- forkLIO $ do
    res      <- (Right `liftM` act) `catchTCB` (return . Left . taintError)
    endLabel <- getLabel
    putLMVarTCB mv $! 
      let le = endLabel `upperBound` l
          m = "End label does not flow to specified upper bound"
          e = VMonitorFailure { monitorFailure = CanFlowToViolation
                              , monitorMessage = m }
      in if canFlowToP p endLabel l
           then res
           else Left $! LabeledExceptionTCB le (toException e)
  return $ LabeledResultTCB { lresThreadIdTCB = tid, lresResultTCB = mv }
    where -- raise the label of the exception to the join of the
          -- exception label and supplied lForkP upper bound
          taintError (LabeledExceptionTCB le e) =
            LabeledExceptionTCB (le `upperBound` l) e

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
lWait :: MonadLIO l m => LabeledResult l a -> m a
lWait = lWaitP noPrivs

-- | Same as 'lWait', but uses priviliges in label checks and raises.
lWaitP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LabeledResult l a -> m a
lWaitP p m = do
  v <- readLMVarP p $ lresResultTCB m
  -- kill thread:
  liftLIO . ioTCB . killThread . lresThreadIdTCB $ m
  case v of
    Right x -> return x
    Left e  -> liftLIO $ unlabeledThrowTCB e

-- | Same as 'lWait', but does not block waiting for result.
trylWait :: MonadLIO l m => LabeledResult l a -> m (Maybe a)
trylWait = trylWaitP noPrivs

-- | Same as 'trylWait', but uses priviliges in label checks and raises.
trylWaitP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LabeledResult l a -> m (Maybe a)
trylWaitP p m = do
  let mvar = lresResultTCB m
  mv <- tryTakeLMVarP p mvar
  case mv of
    Just v -> do putLMVarP p mvar v
                 -- kill thread:
                 liftLIO . ioTCB . killThread . lresThreadIdTCB $ m
                 case v of
                   Right x -> return $! Just x
                   Left e  -> liftLIO $ unlabeledThrowTCB e
    _ -> return Nothing


-- | Suspend current thread for a given number of microseconds.
threadDelay :: MonadLIO l m => Int -> m ()
threadDelay = liftLIO . ioTCB . C.threadDelay

--
-- Forcing computations
--


-- | Though in most cases using a 'LabeledResult' is sufficient, in
-- certain scenarios it is desirable to produce a pure 'Labeled' value
-- that is the result of other potentially sensitive values. As such, we
-- provide @lBracket@.
-- 
-- @lBracket@ is like 'lFork', but rather than returning a
-- 'LabeledResult', it returns a 'Labeled' value. The key difference
-- between the two is that @lBracket@ takes an additional parameter
-- specifying the number of microseconds the inner computation will take.
-- As such, @lBracket@ will block for the specified duration and the
-- result of the inner computation be /forced/. That is, if the
-- computation terminated /cleanly/, i.e., it did not throw an
-- exception and it finished in the time specified, then 'Just' the
-- result is returned, otherwise 'Nothing' is returned.
--
-- Note that the original LIO (before version 0.9) included a similar
-- \"primitive\" called @toLabeled@. We have chosen to call this
-- @lBracket@ in part because it is a more descriptive name and to
-- avoid confusion with the previous @toLabeled@ where time was not 
-- considered.
lBracket :: (MonadLIO l m)
          => l                -- ^ Label of result
          -> Int              -- ^ Duration of computation in microseconds
          -> LIO l a          -- ^ Computation to execute in separate thread
          -> m (Labeled l (Maybe a)) -- ^ Labeled result
lBracket = lBracketP noPrivs

-- | Same as 'lBracket', but uses privileges when forking the new
-- thread.
lBracketP :: (MonadLIO l m, PrivDesc l p)
           => Priv p           -- ^ Privileges
           -> l                -- ^ Label of result
           -> Int              -- ^ Duration of computation in microseconds
           -> LIO l a          -- ^ Computation to execute in separate thread
           -> m (Labeled l (Maybe a)) -- ^ Labeled result
lBracketP p l t act = do
  f <- liftLIO $ lForkP p l act
  threadDelay t
  force f
    where force m = do
            let mv = lresResultTCB m
            -- check to see if it wrote to MVar:
            -- kill thread:
            liftLIO . ioTCB . killThread . lresThreadIdTCB $ m
            v <- tryTakeLMVarTCB mv
            return . labelTCB l =<< case v of
              Just (Right x) -> return (Just x)
              _  -> return Nothing
