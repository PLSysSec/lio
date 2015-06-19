{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module LIO.Monad (
    MonadLIO(..)
  ) where

import safe LIO.Label
import LIO.TCB

-- | Synonym for monad in which 'LIO' is the base monad.
class (Label l, Monad m) => MonadLIO l m | m -> l where
  -- | Lift an 'LIO' computation.
  liftLIO :: LIO l a -> m a

instance Label l => MonadLIO l (LIO l) where
  liftLIO = id
