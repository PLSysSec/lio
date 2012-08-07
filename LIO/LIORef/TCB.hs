{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}
{- |

This module implements the core of labeled 'IORef's in the 'LIO ad.
to "Data.IORef", but the operations take place in the 'LIO' monad.  The
types and functions exported by this module are strictly TCB and do
not perform any information flow checks. The external, safe interface
is provided and documented in "LIO.LIORef".


Different from many labeled objects (e.g., files or MVars), references
are uni-directional. This means that reading from a reference can be
done without being able to write to it; and writing to a refernece can
be done without raising the current label, as if also performing a read.

-}
module LIO.LIORef.TCB (
  LIORef(..)
  -- * Basic Functions
  -- ** Create labeled 'IORef's
  , newLIORefTCB
  -- ** Read 'LIORef's
  , readLIORefTCB
  -- ** Write 'LIORef's
  , writeLIORefTCB
  -- ** Modify 'LIORef's
  , modifyLIORefTCB, atomicModifyLIORefTCB
  ) where

import           Control.Monad.Base

import           LIO
import           LIO.TCB
import           Data.IORef


-- | An @LIORef@ is an @IORef@ with an associated, fixed label.  The
-- restriction to an immutable label come from the fact that it is
-- possible to leak information through the label itself, if we wish to
-- allow @LIORef@ to be an instance of 'LabelOf'.  Of course, you can
-- create an @LIORef@ of 'Labeled' to get a limited form of
-- flow-sensitivity.
data LIORef l a = LIORefTCB { labelOfLIORef :: !l
                            -- ^ Label of the labeled 'IORef'.
                            , unlabelLIORefTCB :: (IORef a)
                            -- ^ Access the underlying 'IORef', ignoring IFC.
                            }

-- | Get the label of an 'LIORef'.
instance LabelOf LIORef where
  labelOf = labelOfLIORef

--
-- Create labeled 'IORef's
--

-- | Trusted constructor that creates labeled references with the
-- given label without any IFC checks.
newLIORefTCB :: MonadLIO l m => l -> a -> m (LIORef l a)
newLIORefTCB l a = do
  ior <- liftBase . ioTCB $! newIORef a
  return $! LIORefTCB l ior

--
-- Write 'LIORef's
--

-- | Trusted function used to read the value of a reference without
-- raising the current label.
readLIORefTCB :: MonadLIO l m => LIORef l a -> m a
readLIORefTCB = liftBase . ioTCB . readIORef . unlabelLIORefTCB

--
-- Write 'LIORef's
--

-- | Trusted function used to write a new value into a labeled
-- reference, ignoring IFC.
writeLIORefTCB :: MonadLIO l m => LIORef l a -> a -> m ()
writeLIORefTCB lr a = liftBase . ioTCB $! writeIORef (unlabelLIORefTCB lr) a

--
-- Modify 'LIORef's
--

-- | Trusted function that mutates the contents on an 'LIORef',
-- ignoring IFC.
modifyLIORefTCB :: MonadLIO l m =>  LIORef l a -> (a -> a) -> m ()
modifyLIORefTCB lr f = liftBase . ioTCB $! modifyIORef (unlabelLIORefTCB lr) f

-- | Trusted function used to atomically modify the contents of a
-- labeled reference, ignoring IFC.
atomicModifyLIORefTCB :: MonadLIO l m => LIORef l a -> (a -> (a, b)) -> m b
atomicModifyLIORefTCB lr f =
  liftBase . ioTCB $! atomicModifyIORef (unlabelLIORefTCB lr) f

