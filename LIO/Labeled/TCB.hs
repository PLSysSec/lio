{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |

A data structure 'Labeled' (labeled value) protects access to pure
values.  Without the appropriate privileges, one cannot produce a
pure value that depends on a secret 'Labeled', or conversely produce a
high-integrity 'Labeled' based on pure data.  This module exports
trusted functions for creating and reading labeled values. See
"LIO.Labeled" for the general API.

-}

module LIO.Labeled.TCB (
    Labeled(..)
  , labelTCB
  ) where

import           Data.Typeable
import           LIO.Label
import           LIO.TCB

-- | @Labeled@ is a type representing labeled data.  
data Labeled l t = LabeledTCB { labelOfLabeled :: !l
                              -- ^ Label of 'Labeled' valued
                              , unlabelTCB     :: !t 
                              -- ^ Extracts the value from an
                              -- 'Labeled', discarding the label and any
                              -- protection.
                              } deriving Typeable

-- | Trusted constructor that creates labeled values.
labelTCB :: Label l => l -> a -> Labeled l a
labelTCB = LabeledTCB


instance (Label l, Show a) => ShowTCB (Labeled l a) where
    showTCB (LabeledTCB l t) = show t ++ " {" ++ show l ++ "}"

instance (Label l, Read l, Read a) => ReadTCB (Labeled l a) where
  readsPrecTCB _ str = do (val, str1) <- reads str
                          ("{", str2) <- lex str1
                          (lab, str3) <- reads str2
                          ("}", rest) <- lex str3
                          return (labelTCB lab val, rest)
