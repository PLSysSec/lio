{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE Trustworthy #-}

{- |

This module exports several guards and taint functions which should be
used by code wishing to implement an IFC-aware API. Specifically, when
associating labels with objects, 'guardAlloc' should be used for
creating/allocating new objects, 'taint' should be used by the
function reading the objects, 'guardWrite' should be used to perform a
write-only (i.e., a write that does not have an implicit read), and
'guardReadWrite' should be used for the more common write case (where
a read is implicit).

-}

module LIO.Guards (
  -- * Allocation
    guardAlloc, guardAllocP
  -- * Write-only
  , guardWrite, guardWriteP
  -- * Read-only
  , taint, taintP
  -- * Read-write / Write-read
  , guardReadWrite, guardReadWriteP
  ) where
import           LIO.Label
import           LIO.Monad
import           LIO.Monad.TCB (updateLIOStateTCB)
import           LIO.Privs
import           LIO.Exception.Throw
import           LIO.Exception.MonitorFailure

import           Control.Monad
import           Control.Monad.Loc

--
-- Allocation
--

-- | Ensures the label argument is between the current IO label and
-- current IO clearance.  Use this function in code that allocates
-- objects--untrusted code shouldn't be able to create an object
-- labeled @l@ unless @guardAlloc l@ does not throw an exception.
--
-- If the label does not flow to clearance 'ClearanceViolation' is
-- thrown; if the current label does not flow to the argument label
-- 'CanFlowToViolation' is thrown.
guardAlloc :: Label l => l -> LIO l ()
guardAlloc = guardAllocP NoPrivs

-- | Like 'guardAlloc', but takes privilege argument to be more permissive.
-- Note: privileges are /only/ used when checking that the current label
-- can flow to the given label.
guardAllocP :: Priv l p => p -> l -> LIO l ()
guardAllocP p newl = do
  c <- getClearance
  l <- getLabel
  unless (newl `canFlowTo` c)  $! throwLIO ClearanceViolation
  unless (canFlowToP p l newl) $! throwLIO CanFlowToViolation

--
-- Read
--

-- | Use @taint l@ in trusted code before observing an object labeled
-- @l@.  This will raise the current label to a value @l'@ such that
-- @l ``canFlowTo`` l'@, or throw @'ClearanceViolation'@ if @l'@ would
-- have to be higher than the current clearance.
taint :: Label l=> l -> LIO l ()
taint = taintP NoPrivs

-- | Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that @taintP@ will never lower the current label.  It
-- simply uses privileges to avoid raising the label as high as 'taint'
-- would raise it.
taintP :: Priv l p => p -> l -> LIO l ()
taintP p newl = do
  c <- getClearance
  l <- getLabel
  let l' = labelDiffP p newl l
  unless (l' `canFlowTo` c) $! throwLIO ClearanceViolation
  updateLIOStateTCB $ \s -> s { lioLabel = l' }

--
-- Write
--


-- | Use @guardWrite l@ in any (trusted) code before writing to an
-- object labeled @l@ for which the write has no observable
-- side-effects.  If @l'@ is the current label, then this function
-- ensures that @l' ``canFlowTo`` l@. If the condition does not hold
-- 'CanFlowToViolation' will be thrown. Note taht @l@ must be below the
-- clearance, otherwise 'ClearanceViolation' will be thrown.
guardWrite :: Label l => l -> LIO l ()
guardWrite = guardWriteP NoPrivs

-- | Like 'guardWrite', but takes privilege argument to be more
-- permissive.
guardWriteP :: Priv l p => p -> l -> LIO l ()
guardWriteP p newl = do
  l <- getLabel
  c <- getClearance
  unless (canFlowToP p l newl) $! throwLIO CanFlowToViolation
  unless (newl `canFlowTo` c)  $! throwLIO ClearanceViolation

-- | Use @guardWrite@ in any (trusted) code before writing to an
-- object labeled @l@ for which a write implies a read. Dually, use 
-- this guard when a read implies a write.
--
-- The implementation of guardWrite is straight forward:
--
-- > guardReadWrite l = guardWrite l >> taint l
--
guardReadWrite :: Label l => l -> LIO l ()
guardReadWrite = guardReadWriteP NoPrivs

-- | Like 'guardReadWrite', but takes privilege argument to be more
-- permissive.
guardReadWriteP :: Priv l p => p -> l -> LIO l ()
guardReadWriteP p newl = do
  guardWriteP p newl
  taintP      p newl
