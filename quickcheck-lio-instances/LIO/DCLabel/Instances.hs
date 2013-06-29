{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- | Instances for "QuickCheck"\'s 'Arbitrary' class.
module LIO.DCLabel.Instances () where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import LIO.DCLabel
import qualified Data.ByteString.Char8 as S8
import LIO.TCB

instance Arbitrary Principal where
  arbitrary = principalBS . S8.singleton <$> elements ['A'..'Z']

instance Arbitrary Disjunction where
  arbitrary = frequency [(1, dFromList <$> arbitrary)
                        , (10, dFromList <$> listOf1 arbitrary)]
-- Reduce the incidence of False to get more interesting tests

instance Arbitrary CNF where
  arbitrary = cFromList <$> arbitrary

instance Arbitrary DCLabel where
  arbitrary = liftA2 DCLabel arbitrary arbitrary

instance Arbitrary DCPriv where
  arbitrary = PrivTCB <$> arbitrary

