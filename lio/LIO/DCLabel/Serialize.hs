{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving,
             GeneralizedNewtypeDeriving #-}
{- |

This module provides instances for binary serialization of
'DCLabel's. Specifically, we provide insgtances for @binary@\'s
@Data.Binary@.

-}
module LIO.DCLabel.Serialize () where


import           LIO.DCLabel.Core
import           Data.Binary
import           Control.Monad

deriving instance Binary Principal
deriving instance Binary Clause

-- | Serialize components by converting them to maybe's
instance Binary Component where
  put c = put . dcToMaybe $! c
    where dcToMaybe DCFalse       = Nothing
          dcToMaybe (DCFormula f) = Just f
  get = dcFromMaybe `liftM` get
    where dcFromMaybe Nothing  = dcFalse
          dcFromMaybe (Just f) = dcFormula f

-- | Serialize labels by converting them to pairs of components.
instance Binary DCLabel where
  put l = put (dcSecrecy l, dcIntegrity l)
  get   = uncurry dcLabelNoReduce `liftM` get
