{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |

This is the TCB-restricted version of "LIO.Labeled", which documents
the implementation of 'Labeled' values and their use. It provides
functions for labeling ('labelTCB') and unlabeling ('unlabelTCB')
labeled values without imposing any information flow restrictions.

-}

module LIO.Labeled.TCB (
    Labeled(..)
  , labelTCB
  ) where

import           Data.Typeable
import           LIO.Label
import           LIO.TCB

-- | @Labeled l a@ is a value that associates a label of type @l@ with
-- a value of type @a@. Labeled values allow users to label data with
-- a label other than the current label. In an embedded setting this
-- is akin to having first class labeled values. Note that 'Labeled'
-- is an instance of 'LabelOf', which effectively means that the label
-- of a 'Labeled' value is usually just protected by the current
-- label. (Of course if you have a nested labeled value then the label
-- on the inner labeled value's label is the outer label.)
data Labeled l t = LabeledTCB { labelOfLabeledTCB :: !l
                              -- ^ Label of 'Labeled' valued
                              , unlabelTCB :: t 
                              -- ^ Extracts the value from an
                              -- 'Labeled', discarding the label and any
                              -- protection.
                              } deriving Typeable
-- Note: unlabelTCB cannot be strict if we want things like lFmap.

-- | Returns label of a 'Labeled' type.
instance LabelOf Labeled where
  labelOf = labelOfLabeledTCB

-- | Trusted constructor that creates labeled values.
labelTCB :: Label l => l -> a -> Labeled l a
labelTCB = LabeledTCB

-- | Trusted 'Show' instance.
instance (Label l, Show a) => ShowTCB (Labeled l a) where
    showTCB (LabeledTCB l t) = show t ++ " {" ++ show l ++ "}"

-- | Trusted 'Read' instance.
instance (Label l, Read l, Read a) => ReadTCB (Labeled l a) where
  readsPrecTCB _ str = do (val, str1) <- reads str
                          ("{", str2) <- lex str1
                          (lab, str3) <- reads str2
                          ("}", rest) <- lex str3
                          return (labelTCB lab val, rest)
