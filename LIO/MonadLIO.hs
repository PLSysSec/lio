{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -XUndecidableInstances #-}

-- |This module provides a function 'liftLIO' for executing 'LIO'
-- computations from transformed versions of the 'LIO' monad.  There
-- is also a method @liftIO@, which is a synonym for 'liftLIO', to
-- help with porting code that expects to run in the 'IO' monad.
-- Users will want to hide or qualify one of the @liftIO@ definitions.
-- For instance:
--
-- > import LIO.Base hiding (MonadLIO(liftIO))
--
module LIO.MonadLIO where

import LIO.TCB

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer

class (Monad m, Label l) => MonadLIO m l s | m -> l s where
    liftLIO :: LIO l s a -> m a
    liftIO  :: LIO l s a -> m a -- Might want to hide this one
    liftIO  = liftLIO

instance (Label l) => MonadLIO (LIO l s) l s where
    liftLIO = id

instance (MonadLIO m l s) => MonadLIO (ContT r m) l s where
    liftLIO = lift . liftLIO
instance (Error e, MonadLIO m l s) => MonadLIO (ErrorT e m) l s where
    liftLIO = lift . liftLIO
instance (MonadLIO m l s) => MonadLIO (ListT m) l s where
    liftLIO = lift . liftLIO
instance (MonadLIO m l s) => MonadLIO (ReaderT r m) l s where
    liftLIO = lift . liftLIO
instance (Monoid w, MonadLIO m l s) => MonadLIO (RWST r w s' m) l s where
    liftLIO = lift . liftLIO
instance (MonadLIO m l s) => MonadLIO (StateT s' m) l s where
    liftLIO = lift . liftLIO
instance (Monoid w, MonadLIO m l s) => MonadLIO (WriterT w m) l s where
    liftLIO = lift . liftLIO
