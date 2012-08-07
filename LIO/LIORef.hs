{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

{- |

Mutable reference in the 'LIO' monad. As with other objects in LIO,
mutable references have an associated label that is used to impose
restrictions on its operations. In fact, labeled references
('LIORef's) are solely labeled 'IORef's with read and write access
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

import           LIO
import           LIO.LIORef.TCB

--
-- Create labeled 'IORef's
--

-- | To create a new reference the label of the reference must be
-- below the thread's current clearance and above the current label.
-- If this is the case, the reference is built. Otherwise an exception
-- will be thrown by the underlying 'guardAlloc' guard.
newLIORef :: MonadLIO l m
          => l                  -- ^ Label of reference
          -> a                  -- ^ Initial value
          -> m (LIORef l a) -- ^ Mutable reference
newLIORef = newLIORefP NoPrivs

-- | Same as 'newLIORef' except @newLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
newLIORefP :: MonadLIOP l p m => p -> l -> a -> m (LIORef l a)
newLIORefP p l a = do
  guardAllocP p l
  newLIORefTCB l a

--
-- Read 'LIORef's
--

-- | Read the value of a labeled reference. A read succeeds only if the
-- label of the reference is below the current clearance. Moreover,
-- the current label is raised to the join of the current label and
-- the reference label. To avoid failures (introduced by the 'taint'
---guard) use 'labelOf' to check that a read will succeed.
readLIORef :: MonadLIO l m => LIORef l a -> m a
readLIORef = readLIORefP NoPrivs

-- | Same as 'readLIORef' except @readLIORefP@ takes a privilege object
-- which is used when the current label is raised.
readLIORefP :: MonadLIOP l p m => p -> LIORef l a -> m a
readLIORefP p lr = do
  taintP p $! labelOf lr
  readLIORefTCB lr

--
-- Write 'LIORef's
--

-- | Write a new value into a labeled reference. A write succeeds if
-- the current label can-flow-to the label of the reference, and the
-- label of the reference can-flow-to the current clearance. Otherwise,
-- an exception is raised by the underlying 'guardAlloc' guard.
writeLIORef :: MonadLIO l m => LIORef l a -> a -> m ()
writeLIORef = writeLIORefP NoPrivs

-- | Same as 'writeLIORef' except @writeLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
writeLIORefP :: MonadLIOP l p m => p -> LIORef l a -> a -> m ()
writeLIORefP p lr a = do
  guardAllocP p $! labelOf lr 
  writeLIORefTCB lr a

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
modifyLIORef = modifyLIORefP NoPrivs

-- | Same as 'modifyLIORef' except @modifyLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
modifyLIORefP :: MonadLIOP l p m =>  p -> LIORef l a -> (a -> a) -> m ()
modifyLIORefP p lr f = do
  guardAllocP p $! labelOf lr 
  modifyLIORefTCB lr f

-- | Atomically modifies the contents of an 'LIORef'. It is required
-- that the label of the reference be above the current label, but
-- below the current clearance. Moreover, since this function can be
-- used to directly read the value of the stored reference, the
-- computation is \"tainted\" by the reference label (i.e., the
-- current label is raised to the 'join' of the current and reference
-- labels). These checks and label raise are done by 'guardWrite',
-- which will raise an exception if any of the IFC conditions cannot
-- be satisfied.
atomicModifyLIORef :: MonadLIO l m => LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORef = atomicModifyLIORefP NoPrivs

-- | Same as 'atomicModifyLIORef' except @atomicModifyLIORefP@ takes
-- a set of privileges which are accounted for in label comparisons.
atomicModifyLIORefP :: MonadLIOP l p m => p -> LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORefP p lr f = do
  guardWriteP p $! labelOf lr 
  atomicModifyLIORefTCB lr f
