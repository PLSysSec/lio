{-# LANGUAGE Trustworthy #-}

-- | This module contains functions to launch 'LIO' computations from
-- within the 'IO' monad.  These functions are not useful from within
-- 'LIO' code (but not harmful either, since their types are in the
-- 'IO' monad).
--
-- This module is intended to be imported into your Main module, for
-- use in invoking 'LIO' code.  The functions are also available via
-- "LIO" and "LIO.Core", but those modules will clutter your namespace
-- with symbols you don't need in the 'IO' monad.
module LIO.Run (LIOState(..), runLIO, tryLIO, evalLIO, privInit) where

import safe Control.Exception
import safe Data.IORef
import safe Data.Typeable

import safe LIO.Label
import LIO.TCB

-- | Execute an 'LIO' action, returning its result and the final label
-- state as a pair.  Note that it returns a pair whether or not the
-- 'LIO' action throws an exception.  Forcing the result value will
-- re-throw the exception, but the label state will always be valid.
--
-- See also 'evalLIO'.
runLIO :: LIO l a -> LIOState l -> IO (a, LIOState l)
runLIO (LIOTCB m) s0 = do
  sp <- newIORef s0
  a <- m sp `catch` \e -> return $ throw $ makeCatchable e
  s1 <- readIORef sp
  return (a, s1)

-- | A variant of 'runLIO' that returns results in 'Right' and
-- exceptions in 'Left', much like the standard library 'try'
-- function.
tryLIO :: LIO l a -> LIOState l -> IO (Either SomeException a, LIOState l)
tryLIO lio s0 = runLIO lio s0 >>= tryit
  where tryit (a, s) = do
          ea <- try (evaluate a)
          return (ea, s)


-- | Given an 'LIO' computation and some initial state, return an IO
-- action which, when executed, will perform the IFC-safe LIO
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

-- | Initialize some privileges (within the 'IO' monad) that can be
-- passed to 'LIO' computations run with 'runLIO' or 'evalLIO'.  This
-- is a pure function, but the result is encapsulated in 'IO' to
-- make the return value inaccessible from 'LIO' computations.
--
-- Note the same effect can be achieved using the 'PrivTCB'
-- constructor, but 'PrivTCB' is easier to misuse is only available by
-- importing "LIO.TCB".
privInit :: (SpeaksFor p) => p -> IO (Priv p)
privInit p | isPriv p  = fail "privInit called on Priv object"
           | otherwise = return $ PrivTCB p

-- | Uses dynamic typing to return 'True' iff the type of the argument
-- is @'Priv' a@ (for any @a@).  Mostly useful to prevent users from
-- accidentally wrapping 'Priv' objects inside other 'Priv' objects.
isPriv :: (Typeable p) => p -> Bool
isPriv p = typeRepTyCon (typeOf p) == privcon
  where privcon = typeRepTyCon $ typeOf noPrivs
