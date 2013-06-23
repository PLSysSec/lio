{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

{- |

Mutable reference in the 'LIO' monad. As with other objects in LIO,
mutable references have an associated label that is used to impose
restrictions on its operations. In fact, labeled references
('LIORef's) are simply labeled 'IORef's with read and write access
restricted according to the label. This module is analogous to
"Data.IORef", but the operations take place in the 'LIO' monad.

-}


module LIO.LIORef (
    LIORef
  -- * Basic Functions
  -- ** Create labeled 'IORef's
  , newLIORef, newLIORefP
  -- ** Read 'LIORef's
  , readLIORef, readLIORefP
  -- ** Write 'LIORef's
  , writeLIORef, writeLIORefP
  -- ** Modify 'LIORef's
  , modifyLIORef, modifyLIORefP
  , atomicModifyLIORef, atomicModifyLIORefP
  ) where

import safe Data.IORef

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label
import LIO.TCB
import LIO.TCB.LObj

--
-- Create labeled 'IORef's
--

-- | An @LIORef@ is an @IORef@ with an associated, fixed label.  The
-- restriction to an immutable label come from the fact that it is
-- possible to leak information through the label itself, if we wish to
-- allow @LIORef@ to be an instance of 'LabelOf'.  Of course, you can
-- create an @LIORef@ of 'Labeled' to get a limited form of
-- flow-sensitivity.
type LIORef l a = LObj l (IORef a)

-- | To create a new reference the label of the reference must be
-- below the thread's current clearance and above the current label.
-- If this is the case, the reference is built. Otherwise an exception
-- will be thrown by the underlying 'guardAlloc' guard.
newLIORef :: Label l
          => l                  -- ^ Label of reference
          -> a                  -- ^ Initial value
          -> LIO l (LIORef l a) -- ^ Mutable reference
newLIORef l a = do
  withContext "newLIORef" $ guardAlloc l
  ioTCB (LObjTCB l `fmap` newIORef a)

-- | Same as 'newLIORef' except @newLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
newLIORefP :: PrivDesc l p => Priv p -> l -> a -> LIO l (LIORef l a)
newLIORefP p l a = do
  withContext "newLIORefP" $ guardAllocP p l
  ioTCB (LObjTCB l `fmap` newIORef a)

--
-- Read 'LIORef's
--

-- | Read the value of a labeled reference. A read succeeds only if the
-- label of the reference is below the current clearance. Moreover,
-- the current label is raised to the join of the current label and
-- the reference label. To avoid failures (introduced by the 'taint'
---guard) use 'labelOf' to check that a read will succeed.
readLIORef :: Label l => LIORef l a -> LIO l a
readLIORef (LObjTCB l r) = do
  withContext "readLIORef" $ taint l
  ioTCB (readIORef r)

-- | Same as 'readLIORef' except @readLIORefP@ takes a privilege object
-- which is used when the current label is raised.
readLIORefP :: PrivDesc l p => Priv p -> LIORef l a -> LIO l a
readLIORefP p (LObjTCB l r) = do
  withContext "readLIORefP" $ taintP p l
  ioTCB (readIORef r)

--
-- Write 'LIORef's
--

-- | Write a new value into a labeled reference. A write succeeds if
-- the current label can-flow-to the label of the reference, and the
-- label of the reference can-flow-to the current clearance. Otherwise,
-- an exception is raised by the underlying 'guardAlloc' guard.
writeLIORef :: Label l => LIORef l a -> a -> LIO l ()
writeLIORef (LObjTCB l r) a = do
  withContext "writeLIORef" $ guardAlloc l
  ioTCB (writeIORef r a)

-- | Same as 'writeLIORef' except @writeLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
writeLIORefP :: PrivDesc l p => Priv p -> LIORef l a -> a -> LIO l ()
writeLIORefP p (LObjTCB l r) a = do
  withContext "writeLIORefP" $ guardAllocP p l
  ioTCB (writeIORef r a)

--
-- Modify 'LIORef's
--

-- | Mutate the contents of a labeled reference. For the mutation to
-- succeed it must be that the current label can flow to the label of the
-- reference, and the label of the reference can flow to the current
-- clearance. Note that because a modifier is provided, the reference
-- contents are not observable by the outer computation and so it is not
-- required that the current label be raised. It is, however, required
-- that the label of the reference be bounded by the current label and
-- clearance (as checked by the underlying 'guardAlloc' guard).
modifyLIORef :: Label l
             =>  LIORef l a            -- ^ Labeled reference
             -> (a -> a)               -- ^ Modifier
             -> LIO l ()
modifyLIORef (LObjTCB l r) f = do
  withContext "modifyLIORef" $ guardAlloc l
  ioTCB (modifyIORef r f)

-- | Same as 'modifyLIORef' except @modifyLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
modifyLIORefP :: PrivDesc l p
              =>  Priv p -> LIORef l a -> (a -> a) -> LIO l ()
modifyLIORefP p (LObjTCB l r) f = do
  withContext "modifyLIORefP" $ guardAllocP p l
  ioTCB (modifyIORef r f)

-- | Atomically modifies the contents of an 'LIORef'. It is required
-- that the label of the reference be above the current label, but
-- below the current clearance. Moreover, since this function can be
-- used to directly read the value of the stored reference, the
-- computation is \"tainted\" by the reference label (i.e., the
-- current label is raised to the 'join' of the current and reference
-- labels). These checks and label raise are done by 'guardWrite',
-- which will raise an exception if any of the IFC conditions cannot
-- be satisfied.
atomicModifyLIORef :: Label l => LIORef l a -> (a -> (a, b)) -> LIO l b
atomicModifyLIORef = blessTCB "atomicModifyIORef" atomicModifyIORef

-- | Same as 'atomicModifyLIORef' except @atomicModifyLIORefP@ takes
-- a set of privileges which are accounted for in label comparisons.
atomicModifyLIORefP :: PrivDesc l p
                    => Priv p -> LIORef l a -> (a -> (a, b)) -> LIO l b
atomicModifyLIORefP = blessPTCB "atomicModifyLIORefP" atomicModifyIORef

