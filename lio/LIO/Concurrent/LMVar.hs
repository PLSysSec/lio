{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}
{- |

This module implements labeled 'MVar's.  The interface is analogous to
"Control.Concurrent.MVar", but the operations take place in the 'LIO'
monad.  A labeled MVar, of type @'LMVar' l a@, is a mutable location
that can be in one of two states; an 'LMVar' can be empty, or it can
be full (with a value of type @a@). The location is protected by a
label of type 'l'.  As in the case of @LIORef@s (see "LIO.LIORef"),
this label is fixed and does not change according to the content
placed into the location.  Unlike @LIORef@s, most operations use
'guardWrite' or 'guardWriteP', reflecting the fact that there is no
such thing as read-only or write-only operations on an 'LMVar'.

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

import safe Control.Concurrent.MVar

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label
import LIO.TCB
import LIO.TCB.LObj

--
-- Creating labeled 'MVar's
--

-- | An @LMVar@ is a labeled synchronization variable (an 'MVar') that
-- can be used by concurrent threads to communicate.
type LMVar l a = LObj l (MVar a)

-- | Create a new labeled MVar, in an empty state. Note that the supplied
-- label must be above the current label and below the current clearance.
-- An exception will be thrown by the underlying 'guardAlloc' if this is
-- not the case.
newEmptyLMVar :: Label l
              => l                -- ^ Label of @LMVar@
              -> LIO l (LMVar l a)    -- ^ New mutable location
newEmptyLMVar l = guardIOTCB (withContext "newEmptyLMVar" $ guardAlloc l) $
  LObjTCB l `fmap` newEmptyMVar

-- | Same as 'newEmptyLMVar' except it takes a set of privileges which
-- are accounted for in comparing the label of the MVar to the current
-- label and clearance.
newEmptyLMVarP :: PrivDesc l p => Priv p -> l -> LIO l (LMVar l a)
newEmptyLMVarP p l =
  guardIOTCB (withContext "newEmptyLMVarP" $ guardAllocP p l) $
    LObjTCB l `fmap` newEmptyMVar

-- | Create a new labeled MVar, in an filled state with the supplied
-- value. Note that the supplied label must be above the current label
-- and below the current clearance.
newLMVar :: Label l
         => l                       -- ^ Label of @LMVar@
         -> a                       -- ^ Initial value of @LMVar@
         -> LIO l (LMVar l a)       -- ^ New mutable location
newLMVar l a = guardIOTCB (withContext "newLMVar" $ guardAlloc l) $
  LObjTCB l `fmap` newMVar a

-- | Same as 'newLMVar' except it takes a set of privileges which are
-- accounted for in comparing the label of the MVar to the current label
-- and clearance.
newLMVarP :: PrivDesc l p => Priv p -> l -> a -> LIO l (LMVar l a)
newLMVarP p l a = guardIOTCB (withContext "newLMVarP" $ guardAllocP p l) $
  LObjTCB l `fmap` newMVar a

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
-- Finally, like 'MVars' if the 'LMVar' is empty, @takeLMVar@
-- blocks.
takeLMVar :: Label l => LMVar l a -> LIO l a
takeLMVar = blessTCB "takeLMVar" takeMVar

-- | Same as 'takeLMVar' except @takeLMVarP@ takes a privilege object
-- which is used when the current label is raised.
takeLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> LIO l a
takeLMVarP = blessPTCB "takeLMVarP" takeMVar

-- | Non-blocking version of 'takeLMVar'. It returns @Nothing@ if the
-- 'LMVar' is empty, otherwise it returns @Just@ value, emptying the
-- 'LMVar'.
tryTakeLMVar :: Label l => LMVar l a -> LIO l (Maybe a)
tryTakeLMVar = blessTCB "tryTakeLMVar" tryTakeMVar

-- | Same as 'tryTakeLMVar', but uses priviliges when raising current
-- label.
tryTakeLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> LIO l (Maybe a)
tryTakeLMVarP = blessPTCB "tryTakeLMVar" tryTakeMVar

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
putLMVar = blessTCB "putLMVar" putMVar

-- | Same as 'putLMVar' except @putLMVarP@ takes a privilege object
-- which is used when the current label is raised.
putLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> a -> LIO l ()
putLMVarP = blessPTCB "putLMVarP" putMVar

-- | Non-blocking version of 'putLMVar'. It returns @True@ if the
-- 'LMVar' was empty and the put succeeded, otherwise it returns @False@.
tryPutLMVar :: Label l => LMVar l a -> a -> LIO l Bool
tryPutLMVar = blessTCB "tryPutLMVar" tryPutMVar

-- | Same as 'tryPutLMVar', but uses privileges when raising current label.
tryPutLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> a -> LIO l Bool
tryPutLMVarP = blessPTCB "tryPutLMVarP" tryPutMVar

--
-- Read 'LMVar'
--

-- | Combination of 'takeLMVar' and 'putLMVar'. Read the value, and
-- just put it back. This operation is not atomic, and can can result
-- in unexpected outcomes if another thread is simultaneously calling
-- a function such as 'putLMVar', 'tryTakeLMVarP', or 'isEmptyLMVar'
-- for this 'LMVar'.
readLMVar :: Label l => LMVar l a -> LIO l a
readLMVar = blessTCB "readLMVar" readMVar

-- | Same as 'readLMVar' except @readLMVarP@ takes a privilege object
-- which is used when the current label is raised.
readLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> LIO l a
readLMVarP = blessPTCB "readLMVarP" readMVar

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
swapLMVar = blessTCB "swapLMVar" swapMVar

-- | Same as 'swapLMVar' except @swapLMVarP@ takes a privilege object
-- which is used when the current label is raised.
swapLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> a -> LIO l a
swapLMVarP = blessPTCB "swapLMVarP" swapMVar

--
-- Check state of 'LMVar'
--

-- | Check the status of an 'LMVar', i.e., whether it is empty. The
-- function succeeds if the label of the 'LMVar' is below the current
-- clearance -- the current label is raised to the join of the 'LMVar'
-- label and the current label.
isEmptyLMVar :: Label l => LMVar l a -> LIO l Bool
isEmptyLMVar = blessTCB "isEmptyLMVar" isEmptyMVar

-- | Same as 'isEmptyLMVar', but uses privileges when raising current label.
isEmptyLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> LIO l Bool
isEmptyLMVarP = blessPTCB "isEmptyLMVarP" isEmptyMVar
