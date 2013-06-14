{-# LANGUAGE Unsafe #-}

{- |

This module exports 'LabeledResult's which are effectively thread exit
results protected by a label. See "LIO.Concurrent" for a description
of the concurrency abstractions of LIO.

-}

module LIO.Concurrent.TCB (
    LabeledResult(..), LResStatus(..), lForkP, lWaitP, trylWaitP, timedlWaitP
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Exception as IO
import Data.IORef

import LIO.Core
import LIO.Label
import LIO.TCB
import LIO.Privs

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

-- | Same as 'lFork', but the supplied set of priviliges are accounted
-- for when performing label comparisons.
lForkP :: PrivDesc l p =>
          Priv p -> l -> LIO l a -> LIO l (LabeledResult l a)
lForkP p l lio = do
  guardAllocP p l
  mv <- ioTCB IO.newEmptyMVar
  st <- ioTCB $ newIORef LResEmpty
  s0 <- getLIOState
  tid <- ioTCB $ IO.mask $ \unmask -> IO.forkIO $ do
    sp <- newIORef s0
    ea <- IO.try $ unmask $ unLIOTCB lio sp
    LIOState lEnd _ <- readIORef sp
    writeIORef st $ case ea of
      _ | not (lEnd `canFlowTo` l) -> LResLabelTooHigh lEnd
      Left e                       -> LResResult $ IO.throw $ makeCatchable e
      Right a                      -> LResResult a
    IO.putMVar mv ()
  return $ LabeledResultTCB tid l mv st


-- | Same as 'lWait', but uses priviliges in label checks and raises.
lWaitP :: PrivDesc l p => Priv p -> LabeledResult l a -> LIO l a
lWaitP p (LabeledResultTCB _ l mv st) = taintP p l >> go
  where go = ioTCB (readIORef st) >>= check
        check LResEmpty = ioTCB (IO.readMVar mv) >> go
        check (LResResult a) = return $! a
        check (LResLabelTooHigh lnew) = do
          modifyLIOStateTCB $ \s -> s {
            lioLabel = partDowngradeP p lnew (lioLabel s) }
          throwLIO ResultExceedsLabel

-- | Same as 'trylWait', but uses priviliges in label checks and raises.
trylWaitP :: PrivDesc l p => Priv p -> LabeledResult l a -> LIO l (Maybe a)
trylWaitP p (LabeledResultTCB _ rl _ st) =
  taintP p rl >> ioTCB (readIORef st) >>= check
  where check LResEmpty = return Nothing
        check (LResResult a) = return . Just $! a
        check (LResLabelTooHigh lnew) = do
          curl <- getLabel
          if canFlowToP p lnew curl
            then throwLIO ResultExceedsLabel
            else return Nothing

-- | A version of 'timedlWait' that takes privileges.  The privileges
-- are used both to downgrade the result (if necessary), and to try
-- catching any 'ResultExceedsLabel' before the timeout period (if
-- possible).
timedlWaitP :: PrivDesc l p => Priv p -> LabeledResult l a -> Int -> LIO l a
timedlWaitP p lr@(LabeledResultTCB t _ mvb _) to = trylWaitP p lr >>= go
  where go (Just a) = return a
        go Nothing = do
          mvk <- ioTCB $ IO.newEmptyMVar
          tk <- ioTCB $ IO.forkIO $ IO.finally (IO.threadDelay to) $
                IO.putMVar mvk () >> IO.throwTo t (Uncatchable IO.ThreadKilled)
          ioTCB $ IO.readMVar mvb
          trylWaitP p lr >>= maybe
            (ioTCB (IO.takeMVar mvk) >> throwLIO ResultExceedsLabel)
            (\a -> ioTCB (IO.killThread tk) >> return a)
