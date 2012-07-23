{-# LANGUAGE Unsafe #-}
{-|
This module provides an implementation for labeled MVars.  A labeled
MVar, of type @'LMVar' l a@, is a mutable location that can be in of
of two states; an 'LMVar' can be empty, or it can be full (with a
value of tye @a@). The location is protected by a label of type 'l'.
As in the case of @LIORef@s (see "LIO.LIORef"), this label is static
and does not change according to the content placed into the location.

'LMVar's can be used to build synchronization primitives and
communication channels ('LMVar's themselves are single-place
channels).  We refer to "Control.Concurrent.MVar" for the full
documentation on MVars.
-}
module LIO.Concurrent.LMVar.TCB (
    LMVar(..)
  -- * Creating labeled 'MVar's
  , newEmptyLMVarTCB, newLMVarTCB
  -- * Take 'LMVar'
  , takeLMVarTCB, tryTakeLMVarTCB
  -- * Put 'LMVar'
  , putLMVarTCB, tryPutLMVarTCB
  -- * Read 'LMVar'
  , readLMVarTCB
  -- * Swap 'LMVar'
  , swapLMVarTCB
  -- * Check state of 'LMVar'
  , isEmptyLMVarTCB
  ) where

import Control.Concurrent.MVar

import LIO.Label
import LIO.Core
import LIO.TCB

-- | An @LMVar@ is a labeled synchronization variable (an 'MVar') that
-- can be used by concurrent threads to communicate.
data LMVar l a = LMVarTCB { labelOfLMVar :: !l
                            -- ^ Label of MVar
                          , unlabelLMVarTCB :: MVar a
                            -- ^ Access the underlying 'MVar'
                          }

instance LabelOf LMVar where
  labelOf = labelOfLMVar

--
-- Creating labeled 'MVar's
--

-- | Trusted function used to create an empty @LMVar@, ignoring IFC.
newEmptyLMVarTCB :: Label l => l -> LIO l (LMVar l a)
newEmptyLMVarTCB l = do
  m <- ioTCB $ newEmptyMVar
  return $ LMVarTCB l m

-- | Trusted function used to create an @LMVar@ with the supplied
-- value, ignoring IFC.
newLMVarTCB :: Label l => l -> a -> LIO l (LMVar l a)
newLMVarTCB l a = do
  m <- ioTCB $ newMVar a
  return $ LMVarTCB l m

--
-- Take 'LMVar'
--

-- | Read the contents of an 'LMVar', ignoring IFC.
takeLMVarTCB :: Label l => LMVar l a -> LIO l a
takeLMVarTCB (LMVarTCB _ m) = ioTCB $ takeMVar m

-- | Same as 'tryTakeLMVar', but ignorses IFC.
tryTakeLMVarTCB :: Label l => LMVar l a -> LIO l (Maybe a)
tryTakeLMVarTCB (LMVarTCB _ m) = ioTCB $ tryTakeMVar m

--
-- Put 'LMVar'
--

-- | Put a value into an 'LMVar', ignoring IFC.
putLMVarTCB :: Label l => LMVar l a -> a -> LIO l ()
putLMVarTCB (LMVarTCB _ m) a = ioTCB $ putMVar m a

-- | Same as 'tryPutLMVar', but ignorses IFC.
tryPutLMVarTCB :: Label l => LMVar l a -> a -> LIO l Bool
tryPutLMVarTCB (LMVarTCB _ m) x = ioTCB $ tryPutMVar m x


--
-- Read 'LMVar'
--

-- | Trusted function used to read (take and put) an 'LMVar', ignoring IFC.
readLMVarTCB :: Label l => LMVar l a -> LIO l a
readLMVarTCB (LMVarTCB _ m) = ioTCB $ readMVar m

--
-- Swap 'LMVar'
--

-- | Trusted function that swaps value of 'LMVar', ignoring IFC.
swapLMVarTCB :: Label l => LMVar l a -> a -> LIO l a
swapLMVarTCB (LMVarTCB _ m) x = ioTCB $ swapMVar m x

--
-- Check state of 'LMVar'
--

-- | Same as 'isEmptyLMVar', but ignorses IFC.
isEmptyLMVarTCB :: Label l => LMVar l a -> LIO l Bool
isEmptyLMVarTCB (LMVarTCB _ m) = ioTCB $ isEmptyMVar m
