{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{- |

This module provides a function 'liftLIO' for executing 'LIO'
computations from transformed versions of the 'LIO' monad.
There is also a method 'liftIO', which is a synonym for 'liftLIO',
to help with porting code that expects to run in the @IO@ monad.

-}

module LIO.MonadLIO ( MonadLIO(..) ) where

import LIO.Label
import LIO.Core

-- | 'MonadIO'-like class.
class (Monad m, Label l) => MonadLIO m l | m -> l where
    -- | Lift 'LIO' computation into the monad
    liftLIO :: LIO l a -> m a
    -- | Synonym for 'liftLIO'
    liftIO  :: LIO l a -> m a
    liftIO  = liftLIO

instance Label l => MonadLIO (LIO l) l where liftLIO = id
