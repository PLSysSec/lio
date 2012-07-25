{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances #-}

{- |

This module provides a function 'liftLIO' for executing 'LIO'
computations from transformed versions of the 'LIO' monad.

-}

module LIO.MonadLIO ( MonadLIO(..) ) where

import LIO.Label
import LIO.Core

-- | 'MonadIO'-like class.
class (Monad m, Label l) => MonadLIO m l | m -> l where
    -- | Lift 'LIO' computation into the monad
    liftLIO :: LIO l a -> m a

instance Label l => MonadLIO (LIO l) l where liftLIO = id
