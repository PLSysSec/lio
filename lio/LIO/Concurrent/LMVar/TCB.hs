{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}
{-|

This module implements the core of labeled 'MVars's in the 'LIO ad.
to "Control.Concurrent.MVar", but the operations take place in the
'LIO' monad.  The types and functions exported by this module are
strictly TCB and do not perform any information flow checks. The
external, safe interface is provided and documented in
"LIO.Concurrent.LMVar".

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

import           Control.Monad.Base
import           Control.Concurrent.MVar
                 
import           LIO.Label
import           LIO.Core
import           LIO.TCB

-- | An @LMVar@ is a labeled synchronization variable (an 'MVar') that
-- can be used by concurrent threads to communicate.
data LMVar l a = LMVarTCB { labelOfLMVar :: !l
                            -- ^ Label of MVar.
                          , unlabelLMVarTCB :: MVar a
                            -- ^ Access the underlying 'MVar', ignoring IFC.
                          }

instance LabelOf LMVar where
  labelOf = labelOfLMVar

--
-- Creating labeled 'MVar's
--

-- | Trusted function used to create an empty @LMVar@, ignoring IFC.
newEmptyLMVarTCB :: MonadLIO l m => l -> m (LMVar l a)
newEmptyLMVarTCB l = do
  m <- liftBase . ioTCB $ newEmptyMVar
  return $ LMVarTCB l m

-- | Trusted function used to create an @LMVar@ with the supplied
-- value, ignoring IFC.
newLMVarTCB :: MonadLIO l m => l -> a -> m (LMVar l a)
newLMVarTCB l a = do
  m <- liftBase . ioTCB $ newMVar a
  return $ LMVarTCB l m

--
-- Take 'LMVar'
--

-- | Read the contents of an 'LMVar', ignoring IFC.
takeLMVarTCB :: MonadLIO l m => LMVar l a -> m a
takeLMVarTCB (LMVarTCB _ m) = liftBase . ioTCB $ takeMVar m

-- | Same as 'tryTakeLMVar', but ignorses IFC.
tryTakeLMVarTCB :: MonadLIO l m => LMVar l a -> m (Maybe a)
tryTakeLMVarTCB (LMVarTCB _ m) = liftBase . ioTCB $ tryTakeMVar m

--
-- Put 'LMVar'
--

-- | Put a value into an 'LMVar', ignoring IFC.
putLMVarTCB :: MonadLIO l m => LMVar l a -> a -> m ()
putLMVarTCB (LMVarTCB _ m) a = liftBase . ioTCB $ putMVar m a

-- | Same as 'tryPutLMVar', but ignorses IFC.
tryPutLMVarTCB :: MonadLIO l m => LMVar l a -> a -> m Bool
tryPutLMVarTCB (LMVarTCB _ m) x = liftBase . ioTCB $ tryPutMVar m x


--
-- Read 'LMVar'
--

-- | Trusted function used to read (take and put) an 'LMVar', ignoring IFC.
readLMVarTCB :: MonadLIO l m => LMVar l a -> m a
readLMVarTCB (LMVarTCB _ m) = liftBase . ioTCB $ readMVar m

--
-- Swap 'LMVar'
--

-- | Trusted function that swaps value of 'LMVar', ignoring IFC.
swapLMVarTCB :: MonadLIO l m => LMVar l a -> a -> m a
swapLMVarTCB (LMVarTCB _ m) x = liftBase . ioTCB $ swapMVar m x

--
-- Check state of 'LMVar'
--

-- | Same as 'isEmptyLMVar', but ignorses IFC.
isEmptyLMVarTCB :: MonadLIO l m => LMVar l a -> m Bool
isEmptyLMVarTCB (LMVarTCB _ m) = liftBase . ioTCB $ isEmptyMVar m
