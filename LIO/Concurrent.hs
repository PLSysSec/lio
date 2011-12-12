{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#endif
-- |This module exposes the concurrent interface to LIO. Specifically,
-- it implements labeled /fork/ ('lFork') and /wait/ ('lWait'). We
-- strongly suggest these primitives over 'toLabeled' as they are
-- termination-senstivie.
module LIO.Concurrent ( lForkP, lFork
                      , lWaitP, lWait
                      ) where

import LIO.TCB
import LIO.Concurrent.LMVar.TCB
import Control.Concurrent
import Control.Exception (toException)
import Data.Functor

-- | An LIO fork.
forkLIO :: (LabelState l s) => LIO l s () -> LIO l s ThreadId
forkLIO m = do
  s <- getTCB
  ioTCB . forkIO $ evalLIO m s >> return ()


-- | Same as 'lFork', but the supplied set of priviliges are accounted
-- for when performing label comparisons.
lForkP :: (Priv l p, LabelState l s, Show a)
       => p -> l -> LIO l s a -> LIO l s (LRes l a)
lForkP p l m = do
  mv <- newEmptyLMVarP p l
  _ <- forkLIO $ do res <- (Right <$> m) `catchTCB` (return . Left)
                    lastL <- getLabel
                    putLMVarTCB mv (if leqp p lastL l
                                      then res
                                      else (mkErr lastL LerrLow))

  return $ LRes mv
    where mkErr cl e = Left . (LabeledExceptionTCB cl) . toException $ e

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
-- supplied label --- doing so will result in an exception being
-- thrown. 
-- 
-- Not that, compared to 'toLabeled', @lFork@ immediately returns a
-- labeled result of type 'LRes', which is essentially a \"future\",
-- or \"promise\". Moreover, to guarantee that the computation has
-- completed, it is important that some thread actually touch the
-- future, i.e., perform an 'lWait'.
lFork :: (LabelState l s, Show a)
      => l                  -- ^ Label of result
      -> LIO l s a          -- ^ Computation to execute in separate thread
      -> LIO l s (LRes l a) -- ^ Labeled result
lFork = lForkP NoPrivs

-- | A labeled thread result is simply a wrapper for a labeled MVar. A
-- thread can observe the result of another thread, only after raising
-- its label to the label of the result.
data LRes l a = LRes (LMVar l (Either (LabeledException l) a))

-- | Same as 'lWait', but uses priviliges in label checks and raises.
lWaitP :: (Priv l p, LabelState l s) => p -> LRes l a -> LIO l s a
lWaitP p (LRes mv) = do
  ea <- takeLMVarP p mv
  case ea of
    Right x -> return x
    Left e -> ioTCB $ throwIO e

-- | Given a labeled result (a future), @lWait@ returns the unwrapped
-- result (blocks, if necessary). For @lWait@ to succeed, the label of
-- the result must be above the current label and below the current
-- clearnce. Moreover, before block-reading, @lWait@ raises the current
-- label to the join of the current label and label of result.
-- If the thread @lWait@ is terminates with an exception (for example
-- if it violates clearance), the exceptin is rethrown. Similarly, if 
-- the thread reads values above the result label, an exception is
-- thrown in place of the result.
lWait :: (LabelState l s) => LRes l a -> LIO l s a
lWait = lWaitP NoPrivs
