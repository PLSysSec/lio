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
import LIO.TCB.LObj

--
-- Create labeled 'IORef's
--

-- | An @LIORef@ is an @IORef@ with an associated, fixed label.  The
-- restriction to an immutable label comes from the fact that it is
-- possible to leak information through the label itself, since we
-- wish to allow @LIORef@ to be an instance of 'LabelOf'.  Of course,
-- you can create an @LIORef@ of 'Labeled' to get a limited form of
-- flow-sensitivity.
type LIORef l a = LObj l (IORef a)

-- | Create a new reference with a particularlabel.  The label
-- specified must be between the thread's current label and clearance,
-- as enforced by 'guardAlloc'.
newLIORef :: (MonadLIO l m, Label l)
          => l                  -- ^ Label of reference
          -> a                  -- ^ Initial value
          -> m (LIORef l a) -- ^ Mutable reference
newLIORef l a = liftLIO . guardIOTCB (withContext "newLIORef" $ guardAlloc l) $
  LObjTCB l `fmap` newIORef a

-- | Same as 'newLIORef' except @newLIORefP@ takes privileges which
-- make the comparison to the current label more permissive, as
-- enforced by 'guardAllocP'.
newLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> a -> m (LIORef l a)
newLIORefP p l a = liftLIO . guardIOTCB (withContext "newLIORefP" $ guardAllocP p l) $
  LObjTCB l `fmap` newIORef a

--
-- Read 'LIORef's
--

-- | Read the value of a labeled reference. A read succeeds only if
-- the label of the reference is below the current
-- clearance. Moreover, the current label is raised to the 'lub' of
-- the current label and the reference label.  To avoid failures
-- (introduced by the 'taint' guard) use 'labelOf' to check that a
-- read will succeed.
readLIORef :: (MonadLIO l m, Label l) => LIORef l a -> m a
readLIORef = liftLIO . blessReadOnlyTCB "readLIORef" readIORef

-- | Same as 'readLIORef', except @readLIORefP@ takes a privilege
-- object which is used when the current label is raised (using
-- 'taintP' instead of 'taint').
readLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LIORef l a -> m a
readLIORefP priv = liftLIO . blessReadOnlyPTCB "readLIORefP" readIORef priv

--
-- Write 'LIORef's
--

-- | Write a new value into a labeled reference. A write succeeds if
-- the current label can-flow-to the label of the reference, and the
-- label of the reference can-flow-to the current clearance. Otherwise,
-- an exception is raised by the underlying 'guardAlloc' guard.
writeLIORef :: (MonadLIO l m, Label l) => LIORef l a -> a -> m ()
writeLIORef ref = liftLIO . blessWriteOnlyTCB "writeLIORef" writeIORef ref

-- | Same as 'writeLIORef' except @writeLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of the
-- reference to the current label.
writeLIORefP :: (MonadLIO l m, PrivDesc l p) => Priv p -> LIORef l a -> a -> m ()
writeLIORefP priv ref = liftLIO . blessWriteOnlyPTCB "writeLIORefP" writeIORef priv ref

--
-- Modify 'LIORef's
--

-- | Mutate the contents of a labeled reference.  The mutation is
-- performed by a a pure function, which, because of laziness, is not
-- actually evaluated until such point as a (possibly higher-labeled)
-- thread actually reads the 'LIORef'.  The caller of @modifyLIORef@
-- learns no information about the previous contents the 'LIORef'.
-- For that reason, there is no need to raise the current label.  The
-- `LIORef`'s label must still lie between the current label and
-- clearance, however (as enforced by 'guardAlloc').
modifyLIORef :: (MonadLIO l m, Label l)
             => LIORef l a             -- ^ Labeled reference
             -> (a -> a)               -- ^ Modifier
             -> m ()
modifyLIORef ref = liftLIO . blessWriteOnlyTCB "modifyLIORef" modifyIORef ref

-- | Like 'modifyLIORef', but takes a privilege argument and guards
-- execution with 'guardAllocP' instead of 'guardAlloc'.
modifyLIORefP :: (MonadLIO l m, PrivDesc l p)
              =>  Priv p -> LIORef l a -> (a -> a) -> m ()
modifyLIORefP priv ref = liftLIO . blessWriteOnlyPTCB "modifyLIORefP" modifyIORef priv ref

-- | Atomically modifies the contents of an 'LIORef'. It is required
-- that the label of the reference be above the current label, but
-- below the current clearance. Moreover, since this function can be
-- used to directly read the value of the stored reference, the
-- computation is \"tainted\" by the reference label (i.e., the
-- current label is raised to the 'join' of the current and reference
-- labels). These checks and label raise are done by 'guardWrite',
-- which will raise a 'LabelError' exception if any of the IFC
-- conditions cannot be satisfied.
atomicModifyLIORef :: (MonadLIO l m, Label l) => LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORef ref = liftLIO . blessTCB "atomicModifyIORef" atomicModifyIORef ref

-- | Same as 'atomicModifyLIORef' except @atomicModifyLIORefP@ takes a
-- set of privileges and uses 'guardWriteP' instead of 'guardWrite'.
atomicModifyLIORefP :: (MonadLIO l m, PrivDesc l p)
                    => Priv p -> LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORefP priv ref = liftLIO . blessPTCB "atomicModifyLIORefP" atomicModifyIORef priv ref
