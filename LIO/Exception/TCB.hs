-- {-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveFunctor,
             GeneralizedNewtypeDeriving,
             DeriveDataTypeable #-}

{- | 

This module exports labeled exceptions that are use throughout 'LIO'
and low-level, /unsafe/, throw and catch primitives.

-}
module LIO.Exception.TCB (
  -- * LIO exceptions
    LabeledException(..)
  -- * Throw and catch
  , unlabeledThrowTCB, catchTCB
  -- * LIO Monad
  , rethrowIoTCB
  ) where

import           Data.Typeable
import           Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
import           Control.Monad.State.Strict
-- import           Control.Monad.Loc
                 
import           LIO.Monad
import           LIO.Monad.TCB
import           LIO.Label

--
-- Exception handling
--

-- | A labeled exception is simply an exception associated with a label.
data LabeledException l = LabeledExceptionTCB l CallTrace SomeException
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
  (res, s1) <- LIOTCB . lift $! (toIO act s0 `E.catch` ioHandler s0)
  putLIOStateTCB s1
  return res
    where toIO io = runStateT (unLIOTCB io)
          ioHandler s e = toIO (handler e) s

-- | Lifts an 'IO' computation into the 'LIO' monad.  If the 'IO' computation
-- throws an exception, it labels the exception with the current label so that the
-- exception can be caught with 'catchLIO'.
rethrowIoTCB :: Label l => IO a -> LIO l a
rethrowIoTCB io = do
  l <- getLabel
  t <- getCallTrace
  ioTCB $ io `E.catch` (E.throwIO . LabeledExceptionTCB l t)
