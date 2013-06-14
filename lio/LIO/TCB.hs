{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{- | 

This module exports 

* The definition of the 'LIO' monad and relevant trusted state
  access/modifying functions.

* Uncatchable exceptions used to pop threads out of the 'LIO' monad
  unconditionally.

* Combinators for executing 'IO' actions.

The documentation and external, safe 'LIO' interface is provided in
"LIO.Core".

-}

module LIO.TCB (
  -- * LIO monad
    LIOState(..), LIO(..), runLIO, evalLIO, MonadLIO(..)
  -- ** Accessing internal state
  , getLIOState, putLIOStateTCB, modifyLIOStateTCB, updateLIOStateTCB 
  -- * Executing IO actions
  , ioTCB
  -- * Exception handling
  ,throwLIO, catch, Uncatchable(..), makeCatchable
  -- * Trusted 'Show' and 'Read'
  , ShowTCB(..), ReadTCB(..)
  ) where

import Control.Applicative
import Control.Exception (Exception(..), SomeException(..))
import qualified Control.Exception as IO
import Control.Monad

import Data.IORef
import Data.Typeable

import Text.Read (minPrec)

import LIO.Label

--
-- LIO Monad
--

-- | Internal state of an 'LIO' computation.
data LIOState l = LIOState { lioLabel     :: !l -- ^ Current label.
                           , lioClearance :: !l -- ^ Current clearance.
                           } deriving (Eq, Show, Read)

-- | The @LIO@ monad is a state monad, with 'IO' as the underlying monad,
-- that carries along a /current label/ ('lioLabel') and /current clearance/
-- ('lioClearance'). The current label imposes restrictions on
-- what the current computation may read and write (e.g., no writes to
-- public channels after reading sensitive data).  Since the current
-- label can be raised to be permissive in what a computation observes,
-- we need a way to prevent certain computations from reading overly
-- sensitive data. This is the role of the current clearance: it imposes
-- an upper bound on the current label.
newtype LIO l a = LIOTCB {
    unLIOTCB :: IORef (LIOState l) -> IO a
  } deriving (Typeable)

instance Monad (LIO l) where
  {-# INLINE return #-}
  return = LIOTCB . const . return
  {-# INLINE (>>=) #-}
  m >>= k = LIOTCB $ \s -> do
    a <- unLIOTCB m s
    unLIOTCB (k a) s
  fail = LIOTCB . const . fail

instance Functor (LIO l) where
  {-# INLINE fmap #-}
  fmap f ma = LIOTCB $ \s -> unLIOTCB ma s >>= return . f

instance Applicative (LIO l) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

-- | Execute an 'LIO' action, returning its result and the final label
-- state as a pair.  Note that it returns a pair whether or not the
-- 'LIO' action throws an exception.  Forcing the result value will
-- re-throw the exception, but the label state will always be valid.
--
-- See also 'evalLIO'.
runLIO :: LIO l a -> LIOState l -> IO (a, LIOState l)
runLIO (LIOTCB m) s0 = do
  sp <- newIORef s0
  a <- m sp `IO.catch` \e -> return $ IO.throw $ makeCatchable e
  s1 <- readIORef sp
  return (a, s1)

-- | Given an 'LIO' computation and some initial state, return an IO
-- action which when executed will perform the IFC-safe LIO
-- computation.
--
-- Because untrusted code cannot execute 'IO' computations, this function
-- should only be useful within trusted code.  No harm is done from
-- exposing the @evalLIO@ symbol to untrusted code.  (In general,
-- untrusted code is free to produce 'IO' computations, but it cannot
-- execute them.)
--
-- Unlike 'runLIO', this function throws an exception if the
-- underlying 'LIO' action terminates with an exception.
evalLIO :: LIO l a -> LIOState l -> IO a
evalLIO lio s = do
  (a, _) <- runLIO lio s
  return $! a

--
-- Monad base
--

-- | Synonym for monad in which 'LIO' is the base monad.
class (Monad m, Label l) => MonadLIO l m | m -> l where
  -- | Lift an 'LIO' computation.
  liftLIO :: LIO l a -> m a

instance Label l => MonadLIO l (LIO l) where
  liftLIO = id

--
-- Internal state
--

-- | Get internal state. This function is not actually unsafe, but
-- to avoid future security bugs we leave all direct access to the
-- internal state to trusted code.
getLIOState :: LIO l (LIOState l)
{-# INLINE getLIOState #-}
getLIOState = LIOTCB readIORef

-- | Set internal state.
putLIOStateTCB :: LIOState l -> LIO l ()
{-# INLINE putLIOStateTCB #-}
putLIOStateTCB s = LIOTCB $ \sp -> writeIORef sp $! s

-- | Update the internal state given some function.
modifyLIOStateTCB :: Label l => (LIOState l -> LIOState l) -> LIO l ()
{-# INLINE modifyLIOStateTCB #-}
modifyLIOStateTCB f = do
  s <- getLIOState
  putLIOStateTCB (f s)

{-# DEPRECATED updateLIOStateTCB "Use modifyLIOStateTCB instead" #-}
updateLIOStateTCB :: Label l => (LIOState l -> LIOState l) -> LIO l ()
updateLIOStateTCB = modifyLIOStateTCB

--
-- Executing IO actions
--

-- | Lifts an 'IO' computation into the 'LIO' monad.  Note that
-- exceptions thrown within the 'IO' computation cannot directly be
-- caught within the 'LIO' computation.  Thus, you will generally want to
-- use 'rethrowIoTCB'.
ioTCB :: IO a -> LIO l a
{-# INLINE ioTCB #-}
ioTCB = LIOTCB . const

--
-- Exception handling
--

-- | An uncatchable exception hierarchy use to terminate an untrusted
-- thread.  Wrap the uncatchable exception in 'Uncatchable' before
-- throwing it to the thread.  'runLIO' will subsequently unwrap the
-- 'Uncatchable' constructor.
--
-- Note this can be circumvented by 'IO.mapException', which should be
-- made unsafe.
data Uncatchable = forall e. (Exception e) => Uncatchable e deriving (Typeable)

instance Show Uncatchable where
  showsPrec p (Uncatchable e) = showsPrec p e

instance Exception Uncatchable where
  toException = SomeException
  fromException (SomeException e) = cast e

-- | Simple utility function that strips 'Uncatchable' from around an
-- exception.
makeCatchable :: SomeException -> SomeException
makeCatchable e@(SomeException einner) =
  case cast einner of Just (Uncatchable enew) -> SomeException enew
                      Nothing                 -> e
                      
throwLIO :: Exception e => e -> LIO l a
throwLIO = ioTCB . IO.throwIO

-- | A simple wrapper around IO catch.  The only subtlety is that code
-- is not allowed to run unless the current label can flow to the
-- current clearance.  Hence, if the label exceeds the clearance, the
-- exception is not caught.  (Only a few conditions such as 'lWait' or
-- raising the clearance within 'scopeClearance' can lead to the label
-- exceeding the clarance, and an exception is always thrown at the
-- time this happens.)
catch :: (Label l, Exception e) => LIO l a -> (e -> LIO l a) -> LIO l a
catch io h =
  LIOTCB $ \s -> unLIOTCB io s `IO.catch` \e -> unLIOTCB (safeh e) s
  where uncatchableType = typeOf (undefined :: Uncatchable)
        safeh e@(SomeException einner) = do
          when (typeOf einner == uncatchableType) $ throwLIO e
          LIOState l c <- getLIOState
          unless (l `canFlowTo` c) $ throwLIO e
          maybe (throwLIO e) h $ fromException e

--
-- Trusted 'Show' and 'Read'
--

-- | It would be a security issue to make certain objects a member of
-- the 'Show' class, but nonetheless it is useful to be able to
-- examine such objects when debugging.  The 'showTCB' method can be used
-- to examine such objects.
class ShowTCB a where
    showTCB :: a -> String

-- | It is useful to have the dual of 'ShowTCB', @ReadTCB@, that allows
-- for the reading of strings that were created using 'showTCB'. Only
-- @readTCB@ (corresponding to 'read') and @readsPrecTCB@ (corresponding
-- to 'readsPrec') are implemented.
class ReadTCB a where
  -- | Trusted 'readsPrec'
  readsPrecTCB :: Int -> ReadS a
  -- | Trusted 'read'
  readTCB :: String -> a
  readTCB str = check $ readsPrecTCB minPrec str
    where check []                          = error "readTCB: no parse"
          check [(x,rst)] | all (==' ') rst = x
                         | otherwise        = error "readTCB: no parse"
          check _                           = error "readTCB: ambiguous parse"
