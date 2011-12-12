{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE SafeImports #-}
#endif
{-|
This module provides an implementation for labeled MVars.  A labeled
MVar, of type @'LMVar' l a@, is a mutable location that can be in
of of two states; an 'LMVar' can be empty, or it can be full (with
a value of tye @a@). The location is protected by a label of type
'l'.  As in the case of @LIORef@s (see "LIO.LIORef"), this label
is static and does not change according to the content placed into
the location.

'LMVar's can be used to build synchronization primitives and
communication channels ('LMVar's themselves are single-place
channels).  We refer to "Control.Concurrent.MVar" for the full
documentation on MVars.
-}
module LIO.Concurrent.LMVar.TCB ( -- * Basic Functions
                                 LMVar
                               , labelOfLMVar
                               , newEmptyLMVar, newEmptyLMVarP
                               , newLMVar, newLMVarP
                               , takeLMVar, takeLMVarP
                               , putLMVar, putLMVarP
                               , readLMVar, readLMVarP
                               , swapLMVar, swapLMVarP
                               , tryTakeLMVar, tryTakeLMVarP
                               , tryPutLMVar, tryPutLMVarP
                               , isEmptyLMVar, isEmptyLMVarP
                               , withLMVar, withLMVarP 
                               -- * Unsafe (TCB) Functions
                               , newEmptyLMVarTCB
                               , newLMVarTCB
                               , takeLMVarTCB
                               , putLMVarTCB
                               , readLMVarTCB
                               , swapLMVarTCB
                               , tryTakeLMVarTCB
                               , tryPutLMVarTCB
                               , isEmptyLMVarTCB
                               , withLMVarTCB
                               ) where

import Control.Concurrent.MVar
import LIO.TCB

-- | An @LMVar@ is a labeled synchronization variable (an 'MVar') that
-- can be used by concurrent threads to communicate.
data LMVar l a = LMVarTCB l (MVar a)

-- | This function returns the label of a labeled MVar.
labelOfLMVar :: Label l => LMVar l a -> l
labelOfLMVar (LMVarTCB l _) = l

-- | Same as 'newEmptyLMVar' except it takes a set of
-- privileges which are accounted for in comparing the label of
-- the MVar to the current label and clearance.
newEmptyLMVarP :: (Priv l p, LabelState l s) => p -> l -> LIO l s (LMVar l a)
newEmptyLMVarP p l = do
  aguardP p l
  iom <- ioTCB $ newEmptyMVar
  return $ LMVarTCB l iom

-- | Create a new labeled MVar, in an empty state. Note that the
-- supplied label must be above the current label and below the current
-- clearance.
newEmptyLMVar :: (LabelState l s)
              => l                      -- ^ Label of @LMVar@
              -> LIO l s (LMVar l a)    -- ^ New mutable location
newEmptyLMVar = newEmptyLMVarP NoPrivs

-- | Trusted function used to create an empty @LMVar@, ignoring IFC.
newEmptyLMVarTCB :: (LabelState l s) => l -> LIO l s (LMVar l a)
newEmptyLMVarTCB l = do
  iom <- ioTCB $ newEmptyMVar
  return $ LMVarTCB l iom

-- | Same as 'newLMVar' except it takes a set of privileges which are
-- accounted for in comparing the label of the MVar to the current label
-- and clearance.
newLMVarP :: (Priv l p, LabelState l s) => p -> l -> a -> LIO l s (LMVar l a)
newLMVarP p l a = do
  aguardP p l
  m <- ioTCB $ newMVar a
  return $ LMVarTCB l m

-- | Create a new labeled MVar, in an filled state with the supplied
-- value. Note that the supplied label must be above the current label
-- and below the current clearance.
newLMVar :: (LabelState l s)
         => l                           -- ^ Label of @LMVar@
         -> a                           -- ^ Initial value of @LMVar@
         -> LIO l s (LMVar l a)         -- ^ New mutable location
newLMVar = newLMVarP NoPrivs

-- | Trusted function used to create an @LMVar@ with the supplied
-- value, ignoring IFC.
newLMVarTCB :: (LabelState l s) => l -> a -> LIO l s (LMVar l a)
newLMVarTCB l a = do
  m <- ioTCB $ newMVar a
  return $ LMVarTCB l m

-- | Same as 'takeLMVar' except @takeLMVarP@ takes a privilege object
-- which is used when the current label is raised.
takeLMVarP :: (Priv l p, LabelState l s) => p -> LMVar l a -> LIO l s a
takeLMVarP p (LMVarTCB l m) = do
  wguardP p l
  ioTCB $ takeMVar m

-- | Return contents of the 'LMVar'. Note that a take consists of a read
-- and a write, since it observes whether or not the 'LMVar' is full,
-- and thus the current label must be the same as that of the
-- 'LMVar' (of course, this is not the case when using privileges).
-- Hence, if the label of the 'LMVar' is below the current clearance,
-- we raise the current label to the join of the current label and the
-- label of the MVar and read the contents of the @MVar@. If the
-- 'LMVar' is empty, @takeLMVar@ blocks.
takeLMVar :: (LabelState l s) => LMVar l a -> LIO l s a
takeLMVar = takeLMVarP NoPrivs

-- | Read the contents of an 'LMVar', ignoring IFC.
takeLMVarTCB :: (LabelState l s) => LMVar l a -> LIO l s a
takeLMVarTCB (LMVarTCB _ m) = ioTCB $ takeMVar m

-- | Same as 'putLMVar' except @putLMVarP@ takes a privilege object
-- which is used when the current label is raised.
putLMVarP :: (Priv l p, LabelState l s) => p -> LMVar l a -> a -> LIO l s ()
putLMVarP p (LMVarTCB l m) a = do
  wguardP p l
  val <- ioTCB $ putMVar m a
  return val

-- | Puts a value into an 'LMVar'. Note that a put consists of a read
-- and a write, since it observes whether or not the 'LMVar' is empty,
-- and so the current label must be the same as that of the 'LMVar'
-- (of course, this is not the case when using privileges). As in the
-- 'takeLMVar' case, if the label of the 'LMVar' is below the current
-- clearance, we raise the current label to the join of the current
-- label and the label of the MVar and put the value into the @MVar@.
-- If the 'LMVar' is full, @putLMVar@ blocks.
putLMVar :: (LabelState l s)
         => LMVar l a   -- ^ Source 'LMVar'
         -> a           -- ^ New value
         -> LIO l s ()
putLMVar = putLMVarP NoPrivs

-- | Put a value into an 'LMVar', ignoring IFC.
putLMVarTCB :: (LabelState l s) => LMVar l a -> a -> LIO l s ()
putLMVarTCB (LMVarTCB _ m) a = ioTCB $ putMVar m a

-- | Same as 'readLMVar' except @readLMVarP@ takes a privilege object
-- which is used when the current label is raised.
readLMVarP :: (Priv l p, LabelState l s) => p -> LMVar l a -> LIO l s a
readLMVarP p (LMVarTCB l m) = do
  wguardP p l
  ioTCB $ readMVar m 

-- | Combination of 'takeLMVar' and 'putLMVar'. Read the value, and just
-- put it back. As specified for 'readMVar', this operation is atomic
-- iff there is no other thread calling 'putLMVar' for this 'LMVar'.
readLMVar :: (LabelState l s) => LMVar l a -> LIO l s a
readLMVar = readLMVarP NoPrivs

-- | Trusted function used to read (take and put) an 'LMVar', ignoring IFC.
readLMVarTCB :: (LabelState l s) => LMVar l a -> LIO l s a
readLMVarTCB (LMVarTCB _ m) = ioTCB $ readMVar m

-- | Same as 'swapLMVar' except @swapLMVarP@ takes a privilege object
-- which is used when the current label is raised.
swapLMVarP :: (Priv l p, LabelState l s) => p -> LMVar l a -> a -> LIO l s a
swapLMVarP p (LMVarTCB l m) x =  do
  wguardP p l
  ioTCB $ swapMVar m x

-- | Takes a value from an 'LMVar', puts a new value into the 'LMvar',
-- and returns the taken value.  Like the other 'LMVar' operations it
-- is required that the label of the 'LMVar' be above the current label
-- and below the current clearance. Moreover, the current label is raised
-- to accommodate for the observation. This operation is atomic iff
-- there is no other thread calling 'putLMVar' for this 'LMVar'.
swapLMVar :: LabelState l s 
          => LMVar l a          -- ^ Source @LMVar@
          -> a                  -- ^ New value
          -> LIO l s a          -- ^ Taken value
swapLMVar = swapLMVarP NoPrivs

-- | Trusted function that swaps value of 'LMVar', ignoring IFC.
swapLMVarTCB :: LabelState l s => LMVar l a -> a -> LIO l s a
swapLMVarTCB (LMVarTCB _ m) x = ioTCB $ swapMVar m x

-- | Same as 'tryTakeLMVar', but uses priviliges when raising current label.
tryTakeLMVarP :: (Priv l p, LabelState l s)
              => p -> LMVar l a -> LIO l s (Maybe a)
tryTakeLMVarP p (LMVarTCB l m) = do
  wguardP p l
  ioTCB $ tryTakeMVar m

-- | Non-blocking version of 'takeLMVar'. It returns @Nothing@ if the
-- 'LMVar' is empty, otherwise it returns @Just@ value, emptying the 'LMVar'.
tryTakeLMVar :: LabelState l s => LMVar l a -> LIO l s (Maybe a)
tryTakeLMVar = tryTakeLMVarP NoPrivs

-- | Same as 'tryTakeLMVar', but ignorses IFC.
tryTakeLMVarTCB :: LabelState l s => LMVar l a -> LIO l s (Maybe a)
tryTakeLMVarTCB (LMVarTCB _ m) = ioTCB $ tryTakeMVar m

-- | Same as 'tryPutLMVar', but uses privileges when raising current label.
tryPutLMVarP :: (Priv l p, LabelState l s)
              => p -> LMVar l a -> a -> LIO l s Bool
tryPutLMVarP p (LMVarTCB l m) x = do
  wguardP p l
  ioTCB $ tryPutMVar m x

-- | Non-blocking version of 'putLMVar'. It returns @True@ if the
-- 'LMVar' was empty and the put succeeded, otherwise it returns @False@.
tryPutLMVar :: LabelState l s => LMVar l a -> a -> LIO l s Bool
tryPutLMVar = tryPutLMVarP NoPrivs

-- | Same as 'tryPutLMVar', but ignorses IFC.
tryPutLMVarTCB :: LabelState l s => LMVar l a -> a -> LIO l s Bool
tryPutLMVarTCB (LMVarTCB _ m) x = ioTCB $ tryPutMVar m x


-- | Same as 'isEmptyLMVar', but uses privileges when raising current label.
isEmptyLMVarP :: (Priv l p, LabelState l s) => p -> LMVar l a -> LIO l s Bool
isEmptyLMVarP p (LMVarTCB l m) = do
  taintP p l
  ioTCB $ isEmptyMVar m

-- | Check the status of an 'LMVar', i.e., whether it is empty. The
-- function succeeds if the label of the 'LMVar' is below the current
-- clearance -- the current label is raised to the join of the 'LMVar'
-- label and the current label. Note that this function only returns a
-- snapshot of the state.
isEmptyLMVar :: LabelState l s => LMVar l a -> LIO l s Bool
isEmptyLMVar = isEmptyLMVarP NoPrivs

-- | Same as 'isEmptyLMVar', but ignorses IFC.
isEmptyLMVarTCB :: LabelState l s => LMVar l a -> LIO l s Bool
isEmptyLMVarTCB (LMVarTCB _ m) = ioTCB $ isEmptyMVar m

-- | Same as 'withLMVar', but uses privileges when performing label
-- comparisons/raises.
withLMVarP :: (Priv l p, LabelState l s)
          => p -> LMVar l a -> (a -> LIO l s b) -> LIO l s b
withLMVarP p m@(LMVarTCB l _) io = do
  wguardP p l
  withLMVarTCB m io

-- | Exception-safe wrapper for working with an 'LMVar'. The original
-- contents of the 'LMVar' will be restored if the supplied action throws
-- an exception. The function is atomic only if there is no other
-- thread that performs a 'putLMVar'.
withLMVar :: LabelState l s => LMVar l a -> (a -> LIO l s b) -> LIO l s b
withLMVar = withLMVarP NoPrivs

-- | Same as 'withLMVar', but ignores IFC.
withLMVarTCB :: LabelState l s => LMVar l a -> (a -> LIO l s b) -> LIO l s b
withLMVarTCB m io =
  mask $ \restore -> do
    a <- takeLMVarTCB m
    b <- restore (io a) `onExceptionTCB` putLMVarTCB m a
    putLMVarTCB m a
    return b
