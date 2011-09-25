{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#else
#warning "This module is not using SafeHaskell"
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- | This module provides a function 'liftLIO' for executing 'LIO'
-- computations from transformed versions of the 'LIO' monad.  There
-- is also a method @liftIO@, which is a synonym for 'liftLIO', to
-- help with porting code that expects to run in the 'IO' monad.
module LIO.MonadLIO where

import LIO.TCB

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
#warning "Did not safely import Control.Monad.Trans"
import Control.Monad.Trans (MonadTrans(..))
#else
import Control.Monad.Trans (MonadTrans(..))
#endif

class (Monad m, Label l) => MonadLIO m l s | m -> l s where
    liftLIO :: LIO l s a -> m a
    liftIO  :: LIO l s a -> m a -- Might want to hide this one
    liftIO  = liftLIO

instance (Label l) => MonadLIO (LIO l s) l s where
    liftLIO = id

instance (MonadLIO m l s, MonadTrans t, Monad (t m)) => MonadLIO (t m) l s where
   liftLIO = lift . liftLIO
