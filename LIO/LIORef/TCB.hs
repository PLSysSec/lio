{-# LANGUAGE Unsafe #-}
-- |This module implements labeled IORefs.  The interface is analogous
-- to "Data.IORef", but the operations take place in the LIO monad.
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

import LIO
import LIO.TCB
import Data.IORef


-- | An @LIORef@ is an @IORef@ with an associated, static label. 
-- The restriction of an immutable label come from the fact that it
-- is possible to leak information  through the label itself.
-- Hence, LIO is /flow-insensitive/. Of course, you can create an
-- @LIORef@ of 'Labeled' to get a limited form of flow-sensitivity.
data LIORef l a = LIORefTCB { labelOfLIORef :: !l
                            -- ^ Label of the labeled 'IORef'.
                            , unlabelLIORefTCB :: (IORef a)
                            -- ^ Access the underlying 'IORef',
                            -- ignoring IFC.
                            }

instance LabelOf LIORef where
  labelOf = labelOfLIORef

--
-- Create labeled 'IORef's
--

-- | Trusted constructor that creates labeled references.
newLIORefTCB :: Label l => l -> a -> LIO l (LIORef l a)
newLIORefTCB l a = do
  ior <- ioTCB $! newIORef a
  return $! LIORefTCB l ior

--
-- Write 'LIORef's
--

-- | Trusted function used to read the value of a reference without
-- raising the current label.
readLIORefTCB :: Label l => LIORef l a -> LIO l a
readLIORefTCB = ioTCB . readIORef . unlabelLIORefTCB

--
-- Write 'LIORef's
--

-- | Trusted function used to write a new value into a labeled
-- reference, ignoring IFC.
writeLIORefTCB :: Label l => LIORef l a -> a -> LIO l ()
writeLIORefTCB lr a = ioTCB $! writeIORef (unlabelLIORefTCB lr) a

--
-- Modify 'LIORef's
--

-- | Trusted function that mutates the contents on an 'LIORef',
-- ignoring IFC.
modifyLIORefTCB :: Label l =>  LIORef l a -> (a -> a) -> LIO l ()
modifyLIORefTCB lr f = ioTCB $! modifyIORef (unlabelLIORefTCB lr) f

-- | Trusted function used to atomically modify the contents of a
-- labeled reference, ignoring IFC.
atomicModifyLIORefTCB :: Label l => LIORef l a -> (a -> (a, b)) -> LIO l b
atomicModifyLIORefTCB lr f = ioTCB $! atomicModifyIORef (unlabelLIORefTCB lr) f

