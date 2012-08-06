{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}
{- |

This module implements labeled 'MVar's.  The interface is analogous to
"Control.Concurrent.MVar", but the operations take place in the 'LIO'
monad.  A labeled MVar, of type @'LMVar' l a@, is a mutable location
that can be in of of two states; an 'LMVar' can be empty, or it can be
full (with a value of tye @a@). The location is protected by a label
of type 'l'.  As in the case of @LIORef@s (see "LIO.LIORef"), this
label is fixed and does not change according to the content placed
into the location.  Different from @LIORef@s, taking and putting
'LMVars' calls the write guard 'guardWrite' to enforce sound
information flow control.

'LMVar's can be used to build synchronization primitives and
communication channels ('LMVar's themselves are single-place
channels).  We refer to "Control.Concurrent.MVar" for the full
documentation on MVars.

-}

module LIO.Concurrent.LMVar (
    LMVar
  -- * Creating labeled 'MVar's
  , newEmptyLMVar, newEmptyLMVarP
  , newLMVar, newLMVarP
  -- * Take 'LMVar'
  , takeLMVar, takeLMVarP
  , tryTakeLMVar, tryTakeLMVarP
  -- * Put 'LMVar'
  , putLMVar, putLMVarP
  -- * Read 'LMVar'
  , tryPutLMVar, tryPutLMVarP
  , readLMVar, readLMVarP
  -- * Swap 'LMVar'
  , swapLMVar, swapLMVarP
  -- * Check state of 'LMVar'
  , isEmptyLMVar, isEmptyLMVarP
  ) where

import           LIO.Label
import           LIO.Core
import           LIO.Privs
import           LIO.Concurrent.LMVar.TCB

--
-- Creating labeled 'MVar's
--

-- | Create a new labeled MVar, in an empty state. Note that the supplied
-- label must be above the current label and below the current clearance.
-- An exception will be thrown by the underlying 'guardAlloc' if this is
-- not the case.
newEmptyLMVar :: MonadLIO l m
              => l                -- ^ Label of @LMVar@
              -> m (LMVar l a)    -- ^ New mutable location
newEmptyLMVar = newEmptyLMVarP NoPrivs

-- | Same as 'newEmptyLMVar' except it takes a set of privileges which
-- are accounted for in comparing the label of the MVar to the current
-- label and clearance.
newEmptyLMVarP :: MonadLIOP l p m => p -> l -> m (LMVar l a)
newEmptyLMVarP p l = do
  guardAllocP p l
  newEmptyLMVarTCB l

-- | Create a new labeled MVar, in an filled state with the supplied
-- value. Note that the supplied label must be above the current label
-- and below the current clearance.
newLMVar :: Label l
         => l                       -- ^ Label of @LMVar@
         -> a                       -- ^ Initial value of @LMVar@
         -> LIO l (LMVar l a)       -- ^ New mutable location
newLMVar = newLMVarP NoPrivs

-- | Same as 'newLMVar' except it takes a set of privileges which are
-- accounted for in comparing the label of the MVar to the current label
-- and clearance.
newLMVarP :: MonadLIOP l p m => p -> l -> a -> m (LMVar l a)
newLMVarP p l a = do
  guardAllocP p l
  newLMVarTCB l a

--
-- Take 'LMVar'
--

-- | Return contents of the 'LMVar'. Note that a take consists of a read
-- and a write, since it observes whether or not the 'LMVar' is full, and
-- thus the current label must be the same as that of the 'LMVar' (of
-- course, this is not the case when using privileges).  Hence, if the
-- label of the 'LMVar' is below the current clearance, we raise the
-- current label to the join of the current label and the label of the
-- MVar and read the contents of the @MVar@. The underlying guard
-- 'guardWrite' will throw an exception if any of the IFC checks fail.
-- If the Finally, like 'MVars' if the 'LMVar' is empty, @takeLMVar@
-- blocks.
takeLMVar :: MonadLIO l m => LMVar l a -> m a
takeLMVar = takeLMVarP NoPrivs

-- | Same as 'takeLMVar' except @takeLMVarP@ takes a privilege object
-- which is used when the current label is raised.
takeLMVarP :: MonadLIOP l p m => p -> LMVar l a -> m a
takeLMVarP p m = do
  guardWriteP p (labelOf m)
  takeLMVarTCB m

-- | Non-blocking version of 'takeLMVar'. It returns @Nothing@ if the
-- 'LMVar' is empty, otherwise it returns @Just@ value, emptying the
-- 'LMVar'.
tryTakeLMVar :: MonadLIO l m => LMVar l a -> m (Maybe a)
tryTakeLMVar = tryTakeLMVarP NoPrivs

-- | Same as 'tryTakeLMVar', but uses priviliges when raising current
-- label.
tryTakeLMVarP :: MonadLIOP l p m => p -> LMVar l a -> m (Maybe a)
tryTakeLMVarP p m = do
  guardWriteP p (labelOf m)
  tryTakeLMVarTCB m

--
-- Put 'LMVar'
--

-- | Puts a value into an 'LMVar'. Note that a put consists of a read and
-- a write, since it observes whether or not the 'LMVar' is empty, and so
-- the current label must be the same as that of the 'LMVar' (of course,
-- this is not the case when using privileges). As in the 'takeLMVar'
-- case, if the label of the 'LMVar' is below the current clearance, we
-- raise the current label to the join of the current label and the label
-- of the MVar and put the value into the @MVar@.  Moreover if these IFC
-- restrictions fail, the underlying 'guardWrite' throws an exception.
-- If the 'LMVar' is full, @putLMVar@ blocks.
putLMVar :: Label l
         => LMVar l a   -- ^ Source 'LMVar'
         -> a           -- ^ New value
         -> LIO l ()
putLMVar = putLMVarP NoPrivs

-- | Same as 'putLMVar' except @putLMVarP@ takes a privilege object
-- which is used when the current label is raised.
putLMVarP :: MonadLIOP l p m => p -> LMVar l a -> a -> m ()
putLMVarP p m a = do
  guardWriteP p (labelOf m)
  putLMVarTCB m a

-- | Non-blocking version of 'putLMVar'. It returns @True@ if the
-- 'LMVar' was empty and the put succeeded, otherwise it returns @False@.
tryPutLMVar :: MonadLIO l m => LMVar l a -> a -> m Bool
tryPutLMVar = tryPutLMVarP NoPrivs

-- | Same as 'tryPutLMVar', but uses privileges when raising current label.
tryPutLMVarP :: MonadLIOP l p m => p -> LMVar l a -> a -> m Bool
tryPutLMVarP p m x = do
  guardWriteP p (labelOf m)
  tryPutLMVarTCB m x

--
-- Read 'LMVar'
--

-- | Combination of 'takeLMVar' and 'putLMVar'. Read the value, and just
-- put it back. As specified for 'readMVar', this operation is atomic
-- iff there is no other thread calling 'putLMVar' for this 'LMVar'.
readLMVar :: MonadLIO l m => LMVar l a -> m a
readLMVar = readLMVarP NoPrivs

-- | Same as 'readLMVar' except @readLMVarP@ takes a privilege object
-- which is used when the current label is raised.
readLMVarP :: MonadLIOP l p m => p -> LMVar l a -> m a
readLMVarP p m = do
  guardWriteP p (labelOf m)
  readLMVarTCB m

--
-- Swap 'LMVar'
--

-- | Takes a value from an 'LMVar', puts a new value into the 'LMvar',
-- and returns the taken value.  Like the other 'LMVar' operations it is
-- required that the label of the 'LMVar' be above the current label and
-- below the current clearance. Moreover, the current label is raised to
-- accommodate for the observation. The underlying 'guardWrite' throws an
-- exception if this cannot be accomplished. This operation is atomic iff
-- there is no other thread calling 'putLMVar' for this 'LMVar'.
swapLMVar :: Label l
          => LMVar l a          -- ^ Source @LMVar@
          -> a                  -- ^ New value
          -> LIO l a            -- ^ Taken value
swapLMVar = swapLMVarP NoPrivs

-- | Same as 'swapLMVar' except @swapLMVarP@ takes a privilege object
-- which is used when the current label is raised.
swapLMVarP :: MonadLIOP l p m => p -> LMVar l a -> a -> m a
swapLMVarP p m x = do
  guardWriteP p (labelOf m)
  swapLMVarTCB m x

--
-- Check state of 'LMVar'
--

-- | Check the status of an 'LMVar', i.e., whether it is empty. The
-- function succeeds if the label of the 'LMVar' is below the current
-- clearance -- the current label is raised to the join of the 'LMVar'
-- label and the current label. Note that this function only returns a
-- snapshot of the state and does not modify it -- hence the
-- underlying guard is 'taint' and not 'guardWrite'.
isEmptyLMVar :: MonadLIO l m => LMVar l a -> m Bool
isEmptyLMVar = isEmptyLMVarP NoPrivs

-- | Same as 'isEmptyLMVar', but uses privileges when raising current label.
isEmptyLMVarP :: MonadLIOP l p m => p -> LMVar l a -> m Bool
isEmptyLMVarP p m = do
  taintP p (labelOf m)
  isEmptyLMVarTCB m
