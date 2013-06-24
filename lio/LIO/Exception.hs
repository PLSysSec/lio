{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{- |

Exception routines much like the 'IO' ones in "Control.Exception" (we
duplicate the documentation below).  There are two differences,
however.  First, LIO does not allow masking of asynchronous exceptions
(since these are relied upon to kill a misbehaving thread).  Hence,
routines like 'onException' are not guaranteed to run if a thread is
unconditionally killed.  Second, in a few cases (such as 'lWait') it
is possible for the current label to be raised above the current
clearance as an exception is thrown, in which case these functions do
not catch the exception, either, since code cannot run under such
circumstances.

-}
module LIO.Exception (
  Exception(..), SomeException(..), throwLIO, catch, handle, try
  , onException, finally, bracket, evaluate
  ) where

import safe Control.Exception (Exception(..), SomeException(..))
import safe qualified Control.Exception as IO
import safe Control.Monad
import safe Data.Typeable

import LIO.TCB
import safe LIO.Label

-- | Throw an exception.
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
catch (LIOTCB io) h =
  LIOTCB $ \s -> io s `IO.catch` \e -> case safeh e of LIOTCB ioe -> ioe s
  where uncatchableType = typeOf (undefined :: UncatchableTCB)
        safeh e@(SomeException einner) = do
          when (typeOf einner == uncatchableType) $ throwLIO e
          LIOState l c <- getLIOStateTCB
          unless (l `canFlowTo` c) $ throwLIO e
          maybe (throwLIO e) h $ fromException e

-- | A version of 'catch' with the arguments swapped around.
handle :: (Label l, Exception e) => (e -> LIO l a) -> LIO l a -> LIO l a
handle = flip catch

-- | Like 'finally', but only performs the final action if there was
-- an exception raised by the computation. 
onException :: Label l => LIO l a -> LIO l b -> LIO l a
onException io cleanup =
  io `catch` \e -> cleanup >> throwLIO (e :: SomeException)

-- | A variant of 'bracket' where the return value from the first
-- computation is not required. 
finally :: Label l => LIO l a -> LIO l b -> LIO l a
finally io cleanup = do
  a <- io `onException` cleanup
  void cleanup
  return a
  


-- | When you want to acquire a resource, do some work with it, and
-- then release the resource, it is a good idea to use @bracket@,
-- because bracket will install the necessary exception handler to
-- release the resource in the event that an exception is raised
-- during the computation. If an exception is raised, then bracket
-- will re-raise the exception (after performing the release). 
bracket :: Label l => LIO l a         -- ^ Computation to run first
                   -> (a -> LIO l c)  -- ^ Computation to run last
                   -> (a -> LIO l b)  -- ^ Computation to run in-between
                   -> LIO l b
bracket before after thing = do
  a <- before
  b <- thing a `onException` after a
  void $ after a
  return b

-- | Forces its argument to be evaluated to weak head normal form when
-- the resultant 'LIO' action is executed.
evaluate :: a -> LIO l a
evaluate = ioTCB . IO.evaluate

-- | Similar to catch, but returns an 'Either' result which is
-- ('Right' @a@) if no exception of type @e@ was raised, or ('Left'
-- @ex@) if an exception of type 'e' was raised and its value is @ex@.
-- If any other type of exception is raised than it will be propogated
-- up to the next enclosing exception handler.
try :: (Label l, Exception a1) => LIO l a -> LIO l (Either a1 a)
try io = fmap Right io `catch` (return . Left)
