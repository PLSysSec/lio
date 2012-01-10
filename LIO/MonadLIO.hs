{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- | This module provides a function 'liftLIO' for executing 'LIO'
-- computations from transformed versions of the 'LIO' monad.
-- There is also a method 'liftIO', which is a synonym for 'liftLIO',
-- to help with porting code that expects to run in the @IO@ monad.
module LIO.MonadLIO (MonadLIO(..)) where

import LIO.TCB (LIO, LabelState)
import Control.Monad.Trans (MonadTrans(..))

-- |  MonadIO-like class.
class (Monad m, LabelState l p s) => MonadLIO m l p s | m -> l p s where
    liftLIO :: LIO l p s a -> m a
    liftIO  :: LIO l p s a -> m a
    liftIO  = liftLIO

instance (LabelState l p s) => MonadLIO (LIO l p s) l p s where
    liftLIO = id

instance (MonadLIO m l p s, MonadTrans t, Monad (t m))
         => MonadLIO (t m) l p s where
   liftLIO = lift . liftLIO
