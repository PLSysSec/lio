{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE SafeImports #-}
#endif
-- |This module implements labeled IORefs.  The interface is analogous
-- to "Data.IORef", but the operations take place in the LIO monad.
module LIO.LIORef.TCB (-- * Basic Functions
                        LIORef
                      , newLIORef, labelOfLIORef
                      , readLIORef, writeLIORef
                      , modifyLIORef
                      , atomicModifyLIORef
                      -- * Privileged Functions
                      , newLIORefP
                      , readLIORefP, writeLIORefP
                      , modifyLIORefP
                      , atomicModifyLIORefP
                      -- * Unsafe (TCB) Functions
                      , newLIORefTCB
                      , readLIORefTCB, writeLIORefTCB
                      , modifyLIORefTCB
                      , atomicModifyLIORefTCB
                      ) where

import LIO.TCB

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
import safe Data.IORef
#else
import Data.IORef
#endif


-- | An @LIORef@ is an @IORef@ with an associated, static label. 
-- The restriction of an immutable label come from the fact that it
-- is possible to leak information  through the label itself.
-- Hence, LIO is /flow-insensitive/. Of course, you can create an
-- @LIORef@ of 'Labeled' to get a limited form of flow-sensitivity.
data LIORef l a = LIORefTCB l (IORef a)


-- | Same as 'newLIORef' except @newLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
newLIORefP :: (Priv l p, LabelState l s)
           => p -> l -> a -> LIO l s (LIORef l a)
newLIORefP p l a = do
  aguardP p l
  ior <- rtioTCB $ newIORef a
  return $ LIORefTCB l ior

-- | To create a new reference the label of the reference must be
-- below the thread's current clearance and above the current label.
-- If this is the case, the reference is built.
newLIORef :: (LabelState l s)
          => l                    -- ^ Label of reference
          -> a                    -- ^ Initial value
          -> LIO l s (LIORef l a) -- ^ Mutable reference
newLIORef = newLIORefP NoPrivs

-- | Trusted constructor that creates labeled references.
newLIORefTCB :: (LabelState l s) => l -> a -> LIO l s (LIORef l a)
newLIORefTCB l a = do
  ior <- rtioTCB $ newIORef a
  return $ LIORefTCB l ior

-- | Get the label of a reference.
labelOfLIORef :: (Label l) => LIORef l a -> l
labelOfLIORef (LIORefTCB l _) = l

-- | Same as 'readLIORef' except @readLIORefP@ takes a privilege object
-- which is used when the current label is raised.
readLIORefP :: (Priv l p, LabelState l s) => p -> LIORef l a -> LIO l s a
readLIORefP p (LIORefTCB l r) = do
  taintP p l 
  rtioTCB $ readIORef r

-- | Read the value of a labeled refernce. A read succeeds only if the
-- label of the reference is below the current clearance. Moreover,
-- the current label is raised to the join of the current label and
-- the reference label. To avoid failures use 'labelOfLIORef' to check
-- that a read will suceed.
readLIORef :: (Label l, LabelState l s) => LIORef l a -> LIO l s a
readLIORef = readLIORefP NoPrivs

-- | Trusted function used to read the value of a reference without
-- raising the current label.
readLIORefTCB :: (LabelState l s) => LIORef l a -> LIO l s a
readLIORefTCB (LIORefTCB _ r) = rtioTCB $ readIORef r

-- | Same as 'writeLIORef' except @writeLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
writeLIORefP :: (Priv l p, LabelState l s)
             => p -> LIORef l a -> a -> LIO l s ()
writeLIORefP p (LIORefTCB l r) a = do
  aguardP p l 
  rtioTCB $ writeIORef r a

-- | Write a new value into a labeled reference. A write succeeds if
-- the current label can-flow-to the label of the reference, and the
-- label of the reference can-flow-to the current clearance.
writeLIORef :: (LabelState l s) => LIORef l a -> a -> LIO l s ()
writeLIORef = writeLIORefP NoPrivs 

-- | Trusted function used to write a new value into a labeled
-- reference, ignoring IFC.
writeLIORefTCB :: (LabelState l s) => LIORef l a -> a -> LIO l s ()
writeLIORefTCB (LIORefTCB _ r) a = rtioTCB $ writeIORef r a

-- | Same as 'modifyLIORef' except @modifyLIORefP@ takes a set of
-- privileges which are accounted for in comparing the label of
-- the reference to the current label and clearance.
modifyLIORefP :: (LabelState l s, Priv l p)
              =>  p -> LIORef l a -> (a -> a) -> LIO l s ()
modifyLIORefP p (LIORefTCB l r) f = do
  aguardP p l 
  rtioTCB $ modifyIORef r f

-- | Mutate the contents of a labeled reference. For the mutation to
-- succeed it must be that the current label can-flow-to the label of
-- the reference, and the label of the reference can-flow-to the
-- current clearance. Note that because a modifer is provided, the
-- reference contents are not observable by the outer computation and
-- so it is not required that the current label be raised.
modifyLIORef :: (LabelState l s)
             =>  LIORef l a            -- ^ Labeled reference
             -> (a -> a)               -- ^ Modifier
             -> LIO l s ()
modifyLIORef = modifyLIORefP NoPrivs

-- | Trusted function that mutates the contents on an 'LIORef',
-- ignoring IFC.
modifyLIORefTCB :: (LabelState l s)
                =>  LIORef l a -> (a -> a) -> LIO l s ()
modifyLIORefTCB (LIORefTCB _ r) f = rtioTCB $ modifyIORef r f


-- | Same as 'atomicModifyLIORef' except @atomicModifyLIORefP@ takes
-- a set of privileges which are accounted for in label comparisons.
atomicModifyLIORefP :: (Priv l p, LabelState l s) =>
                       p -> LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORefP p (LIORefTCB l r) f = do
  aguardP p l
  rtioTCB $ atomicModifyIORef r f

-- | Atomically modifies the contents of an 'LIORef'. It is required
-- that the label of the reference be above the current label, but
-- below the current clearance. 
atomicModifyLIORef :: (LabelState l s) =>
                      LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORef = atomicModifyLIORefP NoPrivs

-- | Trusted function used to atomically modify the contents of a
-- labeled reference, ignoring IFC.
atomicModifyLIORefTCB :: (LabelState l s)
                      => LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORefTCB (LIORefTCB _ r) f = rtioTCB $ atomicModifyIORef r f

