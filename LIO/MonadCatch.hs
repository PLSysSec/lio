{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | This module generalizes 'throw' and 'catch' (from "Control.Exception")
-- to methods that can be defined on multiple monads.
module LIO.MonadCatch (MonadCatch(..), genericBracket) where

import Prelude hiding (catch)
import Control.Exception (Exception, SomeException)
import qualified Control.Exception as E

-- | @MonadCatch@ is the class used to generalize the standard IO
-- @catch@ and @throwIO@ functions to methods that can be defined in
-- multiple monads.
-- Minimal definition requires: @mask@, @throwIO@, @catch@, and
-- @onException@.
class (Monad m) => MonadCatch m where
    -- | Executes a computation with asynchronous exceptions masked.
    -- See "Control.Exception" for more details.
    mask             :: ((forall a. m a -> m a) -> m b)  -- ^ Function
    -- that takes a mask-restoring function as argument and returns an
    -- action to execute.
                     -> m b
    -- | Like 'mask', but does not pass a restore action to the argument.
    mask_            :: m a -> m a
    mask_ io         = mask $ \_ -> io
    -- | A variant of @throwIO@ that can be used within the monad.
    throwIO          :: (Exception e) => e -> m a
    -- | Simplest exception-catching function.
    catch            :: (Exception e) => m a        -- ^ Computation to run 
                                      -> (e -> m a) -- ^ Handler
                                      -> m a
    -- | Version of 'catch' with the arguments swapped around.
    handle           :: (Exception e) => (e -> m a) -> m a -> m a
    handle           = flip catch
    -- | Performs an action and a subsequent action if an exceptino is raised.
    onException      :: m a  -- ^ Computation to run first
                     -> m b  -- ^ Computation to run after, if
                             -- an exception was raised.
                     -> m a
    onException io h = io `catch` \e -> h >> throwIO (e :: SomeException)
    -- | This function allows you to execute an action with an initial
    -- \"acquire resource\" and final \"release resource\" as @bracket@
    -- of "Control.Exception".
    bracket          :: m b        -- ^ Computation to run first
                     -> (b -> m c) -- ^ Computation to run last
                     -> (b -> m a) -- ^ Computation to run in-between
                     -> m a
    bracket          = genericBracket onException
    -- | Variant of 'bracket' where the return value from the first
    -- computation is not required. 
    bracket_         :: m a -> m b -> m c -> m c
    bracket_ a b c   = bracket a (const b) (const c)
    -- | Performs an action and a subsequent action.
    finally          :: m a -- ^ Computation to run first
                     -> m b -- ^ Computation to run after
                     -> m a
    finally a b      = mask $ \restore -> do
                         r <- restore a `onException` b
                         _ <- b
                         return r

instance MonadCatch IO where
    mask        = E.mask
    throwIO     = E.throwIO
    catch       = E.catch
    onException = E.onException
    bracket     = E.bracket

-- | Given some general @onException@ function, @genericBracket@
-- allows you to execute an action with an initial \"acquire resource\"
-- and final \"release resource\" as @bracket@ of "Control.Exception".
genericBracket :: (MonadCatch m) =>
                  (m b -> m c -> m b) -- ^ On exception function
               -> m a                 -- ^ Action to perform first
               -> (a -> m c)          -- ^ Action to perform last
               -> (a -> m b)          -- ^ Action to perform in-between
               -> m b                 -- ^ Result of in-between action
genericBracket myOnException m1 m3 m2 =
    mask $ \restore -> do
      a <- m1
      b <- restore (m2 a) `myOnException` (m3 a)
      _ <- m3 a
      return b
