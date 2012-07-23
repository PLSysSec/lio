{-# LANGUAGE Trustworthy #-}
{-

This module implements labeled @MVar@s.  The interface is analogous to
"Control.Concurrent.MVar", but the operations take place in the LIO
monad.  Moreover, taking and putting @MVars@ calls a write guard
@wguard@ as every read implies a write and vice versa. This module
exports only the safe subset (non-TCB) of the @LMVar@ module trusted
code can import "LIO.Concurrent.LMVar.TCB". 

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

import LIO.Label
import LIO.Core
import LIO.Privs
import LIO.Concurrent.LMVar.TCB

--
-- Creating labeled 'MVar's
--

-- | Create a new labeled MVar, in an empty state. Note that the
-- supplied label must be above the current label and below the current
-- clearance.
newEmptyLMVar :: Label l
              => l                    -- ^ Label of @LMVar@
              -> LIO l (LMVar l a)    -- ^ New mutable location
newEmptyLMVar = newEmptyLMVarP NoPrivs

-- | Same as 'newEmptyLMVar' except it takes a set of
-- privileges which are accounted for in comparing the label of
-- the MVar to the current label and clearance.
newEmptyLMVarP :: Priv l p => p -> l -> LIO l (LMVar l a)
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
newLMVarP :: Priv l p => p -> l -> a -> LIO l (LMVar l a)
newLMVarP p l a = do
  guardAllocP p l
  newLMVarTCB l a

--
-- Take 'LMVar'
--

-- | Return contents of the 'LMVar'. Note that a take consists of a read
-- and a write, since it observes whether or not the 'LMVar' is full,
-- and thus the current label must be the same as that of the
-- 'LMVar' (of course, this is not the case when using privileges).
-- Hence, if the label of the 'LMVar' is below the current clearance,
-- we raise the current label to the join of the current label and the
-- label of the MVar and read the contents of the @MVar@. If the
-- 'LMVar' is empty, @takeLMVar@ blocks.
takeLMVar :: Label l => LMVar l a -> LIO l a
takeLMVar = takeLMVarP NoPrivs

-- | Same as 'takeLMVar' except @takeLMVarP@ takes a privilege object
-- which is used when the current label is raised.
takeLMVarP :: Priv l p => p -> LMVar l a -> LIO l a
takeLMVarP p m = do
  guardWriteP p (labelOf m)
  takeLMVarTCB m

-- | Non-blocking version of 'takeLMVar'. It returns @Nothing@ if the
-- 'LMVar' is empty, otherwise it returns @Just@ value, emptying the 'LMVar'.
tryTakeLMVar :: Label l => LMVar l a -> LIO l (Maybe a)
tryTakeLMVar = tryTakeLMVarP NoPrivs

-- | Same as 'tryTakeLMVar', but uses priviliges when raising current label.
tryTakeLMVarP :: Priv l p => p -> LMVar l a -> LIO l (Maybe a)
tryTakeLMVarP p m = do
  guardWriteP p (labelOf m)
  tryTakeLMVarTCB m

--
-- Put 'LMVar'
--

-- | Puts a value into an 'LMVar'. Note that a put consists of a read
-- and a write, since it observes whether or not the 'LMVar' is empty,
-- and so the current label must be the same as that of the 'LMVar'
-- (of course, this is not the case when using privileges). As in the
-- 'takeLMVar' case, if the label of the 'LMVar' is below the current
-- clearance, we raise the current label to the join of the current
-- label and the label of the MVar and put the value into the @MVar@.
-- If the 'LMVar' is full, @putLMVar@ blocks.
putLMVar :: Label l
         => LMVar l a   -- ^ Source 'LMVar'
         -> a           -- ^ New value
         -> LIO l ()
putLMVar = putLMVarP NoPrivs

-- | Same as 'putLMVar' except @putLMVarP@ takes a privilege object
-- which is used when the current label is raised.
putLMVarP :: Priv l p => p -> LMVar l a -> a -> LIO l ()
putLMVarP p m a = do
  guardWriteP p (labelOf m)
  putLMVarTCB m a

-- | Non-blocking version of 'putLMVar'. It returns @True@ if the
-- 'LMVar' was empty and the put succeeded, otherwise it returns @False@.
tryPutLMVar :: Label l => LMVar l a -> a -> LIO l Bool
tryPutLMVar = tryPutLMVarP NoPrivs

-- | Same as 'tryPutLMVar', but uses privileges when raising current label.
tryPutLMVarP :: Priv l p => p -> LMVar l a -> a -> LIO l Bool
tryPutLMVarP p m x = do
  guardWriteP p (labelOf m)
  tryPutLMVarTCB m x

--
-- Read 'LMVar'
--

-- | Combination of 'takeLMVar' and 'putLMVar'. Read the value, and just
-- put it back. As specified for 'readMVar', this operation is atomic
-- iff there is no other thread calling 'putLMVar' for this 'LMVar'.
readLMVar :: Label l => LMVar l a -> LIO l a
readLMVar = readLMVarP NoPrivs

-- | Same as 'readLMVar' except @readLMVarP@ takes a privilege object
-- which is used when the current label is raised.
readLMVarP :: Priv l p => p -> LMVar l a -> LIO l a
readLMVarP p m = do
  guardWriteP p (labelOf m)
  readLMVarTCB m

--
-- Swap 'LMVar'
--

-- | Takes a value from an 'LMVar', puts a new value into the 'LMvar',
-- and returns the taken value.  Like the other 'LMVar' operations it
-- is required that the label of the 'LMVar' be above the current label
-- and below the current clearance. Moreover, the current label is raised
-- to accommodate for the observation. This operation is atomic iff
-- there is no other thread calling 'putLMVar' for this 'LMVar'.
swapLMVar :: Label l
          => LMVar l a          -- ^ Source @LMVar@
          -> a                  -- ^ New value
          -> LIO l a            -- ^ Taken value
swapLMVar = swapLMVarP NoPrivs

-- | Same as 'swapLMVar' except @swapLMVarP@ takes a privilege object
-- which is used when the current label is raised.
swapLMVarP :: Priv l p => p -> LMVar l a -> a -> LIO l a
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
-- snapshot of the state.
isEmptyLMVar :: Label l => LMVar l a -> LIO l Bool
isEmptyLMVar = isEmptyLMVarP NoPrivs

-- | Same as 'isEmptyLMVar', but uses privileges when raising current label.
isEmptyLMVarP :: Priv l p => p -> LMVar l a -> LIO l Bool
isEmptyLMVarP p m = do
  taintP p (labelOf m)
  isEmptyLMVarTCB m
