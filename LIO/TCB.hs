{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveFunctor,
             RankNTypes,
             GeneralizedNewtypeDeriving,
             DeriveDataTypeable #-}

{- | 

This module exports 

* The definition of the 'LIO' monad and relevant trusted state
  access/modifying functions.

* Labeled exceptions that are use throughout 'LIO' and low-level,
  /unsafe/, throw and catch primitives.

* Combinators for executing 'IO' actions.

-}

module LIO.TCB (
  -- * LIO monad
    LIO(..)
  -- ** Internal state
  , LIOState(..)
  , getLIOStateTCB, putLIOStateTCB, updateLIOStateTCB 
  -- * Exceptions
  , LabeledException(..)
  -- ** Throw and catch
  , unlabeledThrowTCB, catchTCB
  -- * Executing IO actions
  , ioTCB, rethrowIoTCB
  -- * Trusted 'Show' and 'Read'
  , ShowTCB(..), ReadTCB(..)
  ) where

import           Data.Typeable

import           Control.Applicative
import           Control.Monad.State.Strict
import           Control.Exception (Exception, SomeException)
import qualified Control.Exception as E

import           Text.Read (minPrec)

import           LIO.Label

--
-- LIO Monad
--

-- | Internal state of an 'LIO' computation.
data LIOState l = LIOState {
    lioLabel     :: !l         -- ^ Current label
  , lioClearance :: !l         -- ^ Current clearance
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
--
-- Finally, to improve debugging and present meaningful error messages
-- the underlying state keeps a call trace (using @monadloc@).
newtype LIO l a = LIOTCB { unLIOTCB :: StateT (LIOState l) IO a }
  deriving (Functor, Applicative, Monad)

--
-- Internal state
--

-- | Get internal state. This function is not actually unsafe, but
-- to avoid future security bugs we leave all direct access to the
-- internal state to trusted code.
getLIOStateTCB :: Label l => LIO l (LIOState l)
getLIOStateTCB = LIOTCB . StateT $! \s -> return (s, s)

-- | Set internal state.
putLIOStateTCB :: Label l => LIOState l -> LIO l ()
putLIOStateTCB s = LIOTCB . StateT $! \_ -> return ((), s)

-- | Update the internal state given some function.
updateLIOStateTCB :: Label l => (LIOState l -> LIOState l) -> LIO l ()
updateLIOStateTCB f = do
  s <- getLIOStateTCB
  putLIOStateTCB $! f s


--
-- Exceptions
--

-- | A labeled exception is simply an exception associated with a label.
data LabeledException l = LabeledExceptionTCB !l SomeException
  deriving (Show, Typeable)

instance Label l => Exception (LabeledException l)

-- | Throw an arbitrary exception. Note that the exception being
-- thrown is not labeled.
unlabeledThrowTCB :: (Exception e, Label l) => e -> LIO l a
unlabeledThrowTCB = LIOTCB . liftIO . E.throwIO

-- | Catch an exception. Note that all exceptions thrown by LIO are
-- labeled and thus this trusted function can be used to handle any
-- exception. Note that the label of the exception must be considered
-- in the handler (i.e., handler must raise the current label) to
-- preserve security.
catchTCB :: Label l
         => LIO l a
         -> (LabeledException l -> LIO l a)
         -> LIO l a
catchTCB act handler = do
  s0 <- getLIOStateTCB
  (res, s1) <- ioTCB $! toIO act s0 `E.catch` ioHandler s0
  putLIOStateTCB s1
  return res
    where toIO io = runStateT (unLIOTCB io)
          ioHandler s e = toIO (handler e) s

--
-- Executing IO actions
--

-- | Lifts an 'IO' computation into the 'LIO' monad.  Note that
-- exceptions thrown within the 'IO' computation cannot directly be
-- caught within the 'LIO' computation.  Thus, you will generally want to
-- use 'rtioTCB' exported by "LIO.Exception.TCB" instead of 'ioTCB'.
ioTCB :: Label l => IO a -> LIO l a
ioTCB = LIOTCB . lift

-- | Lifts an 'IO' computation into the 'LIO' monad.  If the 'IO'
-- computation throws an exception, it labels the exception with the
-- current label so that the exception can be caught with 'catchLIO'.
rethrowIoTCB :: Label l => IO a -> LIO l a
rethrowIoTCB io = do
  l <- lioLabel `liftM` getLIOStateTCB
  ioTCB $ io `E.catch` (E.throwIO . LabeledExceptionTCB l)


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
