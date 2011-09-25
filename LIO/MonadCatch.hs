{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#else
#warning "This module is not using SafeHaskell"
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module generalizes 'throw' and 'catch' (from
-- "Control.Exception") to methods that can be defined on multiple
-- Monads.
module LIO.MonadCatch (MonadCatch(..), genericBracket) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
import safe Prelude hiding (catch)
import safe Control.Exception (Exception, SomeException)
import safe qualified Control.Exception as E
#else
import Prelude hiding (catch)
import Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
#endif

-- | @MonadCatch@ is the class used to generalize the standard IO
-- @catch@ and @throwIO@ functions to methods that can be defined in
-- multiple monads.
class (Monad m) => MonadCatch m where
    block            :: m a -> m a
    unblock          :: m a -> m a
    throwIO          :: (Exception e) => e -> m a
    catch            :: (Exception e) => m a -> (e -> m a) -> m a
    handle           :: (Exception e) => (e -> m a) -> m a -> m a
    handle           = flip catch
    onException      :: m a -> m b -> m a
    onException io h = io `catch` \e -> h >> throwIO (e :: SomeException)
    bracket          :: m b -> (b -> m c) -> (b -> m a) -> m a
    bracket          = genericBracket onException

instance MonadCatch IO where
    block       = E.block
    unblock     = E.unblock
    throwIO     = E.throwIO
    catch       = E.catch
    onException = E.onException
    bracket     = E.bracket

genericBracket :: (MonadCatch m) =>
                  (m b -> m c -> m b) -- ^ On exception function
               -> m a                 -- ^ Action to perform before
               -> (a -> m c)          -- ^ Action for afterwards
               -> (a -> m b)          -- ^ Main (in between) action
               -> m b                 -- ^ Result of main action
genericBracket myOnException before after between =
    block $ do
      a <- before
      b <- unblock (between a) `myOnException` after a
      after a >> return b
