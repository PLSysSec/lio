{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Exception routines much like the 'IO' ones in
-- "Control.Exception".  There are two differences, however.  First,
-- LIO does not allow masking of asynchronous exceptions (since these
-- are relied upon to kill a misbehaving thread).  Hence, routines
-- like 'onException' are not guaranteed to run if a thread is
-- unconditionally killed.  Second, in a few cases (such as 'lWait')
-- it is possible for the current label to be raised above the current
-- clearance as an exception is thrown, in which case these functions
-- do not catch the exception, either, since code cannot run under
-- such circumstances.
module LIO.Exception (
  Exception(..), SomeException(..), throwLIO, catch, try
  , onException, finally, bracket, evaluate
  ) where

import Control.Exception (Exception(..), SomeException(..))
import qualified Control.Exception as IO
import Control.Monad
import Data.Typeable

import LIO.TCB
import LIO.Label

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
  where uncatchableType = typeOf (undefined :: UncatchableTCB)
        safeh e@(SomeException einner) = do
          when (typeOf einner == uncatchableType) $ throwLIO e
          LIOState l c <- getLIOStateTCB
          unless (l `canFlowTo` c) $ throwLIO e
          maybe (throwLIO e) h $ fromException e

onException :: Label l => LIO l a -> LIO l b -> LIO l a
onException io cleanup =
  io `catch` \e -> cleanup >> throwLIO (e :: SomeException)

finally :: Label l => LIO l a -> LIO l b -> LIO l a
finally io cleanup = do
  a <- io `onException` cleanup
  void cleanup
  return a
  
bracket :: Label l => LIO l a -> (a -> LIO l c) -> (a -> LIO l b) -> LIO l b
bracket before after thing = do
  a <- before
  b <- thing a `onException` after a
  void $ after a
  return b

evaluate :: a -> LIO l a
evaluate = ioTCB . IO.evaluate

try :: (Label l, Exception a1) => LIO l a -> LIO l (Either a1 a)
try io = fmap Right io `catch` (return . Left)
