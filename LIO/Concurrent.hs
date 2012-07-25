{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |This module exposes the concurrent interface to LIO. Specifically,
-- it implements labeled /fork/ ('lFork') and /wait/ ('lWait').
module LIO.Concurrent (
    LabeledResult, ThreadId
  -- * Forking new threads
  , forkLIO, lForkP, lFork
  -- * Waiting on threads
  , lWaitP, lWait
  -- * EXPERIMENTAL: Forcing computations
  , ForcedTermination(..)
  , lForce, lForceP
  ) where

import Data.Typeable

import Control.Monad
import Control.Concurrent
import Control.Exception (toException, Exception)

import LIO.Label
import LIO.Core
import LIO.Labeled
import LIO.Labeled.TCB
import LIO.Privs
import LIO.TCB

import LIO.Concurrent.TCB
import LIO.Concurrent.LMVar
import LIO.Concurrent.LMVar.TCB (putLMVarTCB)

--
-- Fork
--

-- | Execute an 'LIO' computation in a new thread.
forkLIO :: Label l => LIO l () -> LIO l ThreadId
forkLIO act = do
  s <- getLIOStateTCB
  ioTCB . forkIO . void $ tryLIO act s


-- | Labeled fork. @lFork@ allows one to invoke computations taht
-- would otherwise raise the current label, but without actually
-- raising the label. The computation is executed in a separate thread
-- and writes its result into a labeled result (whose label is
-- supplied). To observe the result of the computation, or if the
-- computation has terminated, one will have to call 'lWait' and
-- raise the current label. Of couse, as in 'toLabeled', this can be
-- postponed until the result is needed.
--
-- @lFork@ takes a label, which corresponds to the label of the
-- result. It is require that this label is above the current label,
-- and below the current clearance. Moreover, the supplied computation
-- must not read anything more sensitive, i.e., with a label above the
-- supplied label --- doing so will result in an exception (whose
-- label will reflect this observation) being thrown. 
--
-- If an exception is thrown in the inner computation, the exception
-- label will be raised to the join of the result label and original
-- exception label.
-- 
-- Not that, compared to 'toLabeled', @lFork@ immediately returns a
-- labeled result of type 'LRes', which is essentially a \"future\",
-- or \"promise\". Moreover, to guarantee that the computation has
-- completed, it is important that some thread actually touch the
-- future, i.e., perform an 'lWait'.
lFork :: Label l
      => l                -- ^ Label of result
      -> LIO l a          -- ^ Computation to execute in separate thread
      -> LIO l (LabeledResult l a) -- ^ Labeled result
lFork = lForkP NoPrivs

-- | Same as 'lFork', but the supplied set of priviliges are accounted
-- for when performing label comparisons.
lForkP :: Priv l p
       => p -> l -> LIO l a -> LIO l (LabeledResult l a)
lForkP p l act = do
  -- Upperbound is between current label and clearance, asserted by
  -- 'newEmptyLMVarP', otherwise add: guardAllocP p l
  mv <- newEmptyLMVarP p l
  tid <- forkLIO $ do
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

-- | Given a labeled result (a future), @lWait@ returns the unwrapped
-- result (blocks, if necessary). For @lWait@ to succeed, the label of
-- the result must be above the current label and below the current
-- clearnce. Moreover, before block-reading, @lWait@ raises the current
-- label to the join of the current label and label of result.
-- If the thread @lWait@ is terminates with an exception (for example
-- if it violates clearance), the exceptin is rethrown. Similarly, if 
-- the thread reads values above the result label, an exception is
-- thrown in place of the result.
lWait :: Label l => LabeledResult l a -> LIO l a
lWait = lWaitP NoPrivs

-- | Same as 'lWait', but uses priviliges in label checks and raises.
lWaitP :: Priv l p => p -> LabeledResult l a -> LIO l a
lWaitP p m = do
  v <- readLMVarP p $ lresResultTCB m
  case v of
    Right x -> return x
    Left e  -> unlabeledThrowTCB e

--
-- Forcing computations
--

-- | Exception thrown to LIO-forked threads to terminate
data ForcedTermination = ForcedTermination deriving (Show, Typeable)
instance Exception ForcedTermination 

-- | Force execution of a the thread spawned with 'lFork' or 'lForkP'.
-- If the computation completed without raising an exception, then the
-- result is wrapped in a 'Just', otherwise this function returns
-- 'Nothing'. Note that the current computation must be able to read
-- and write at the security level of the labeled result.
lForce :: Label l => LabeledResult l a -> LIO l (Labeled l (Maybe a))
lForce = lForceP NoPrivs

-- | Same as 'lForce', but uses privileges when raising current label
-- to the join of the current label and result label.
lForceP :: Priv l p => p -> LabeledResult l a -> LIO l (Labeled l (Maybe a))
lForceP p m = do
  let l = labelOf m
      mv = lresResultTCB m
  guardWriteP p l
  -- kill thread:
  ioTCB $ throwTo (lresThreadIdTCB m) ForcedTermination
  -- check to see if it wrote to MVar
  v <- tryTakeLMVarP p mv
  return . labelTCB l =<< case v of
    Nothing -> do
      let e = toException ForcedTermination
      putLMVarTCB mv $ Left (LabeledExceptionTCB l e)
      return Nothing
    Just v' -> do
      putLMVarTCB mv v'
      case v' of
        Left _  -> return Nothing
        Right x -> return (Just x)
