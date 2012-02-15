{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Monadic
import Control.Monad (liftM)
import LIO.TCB
import LIO.DCLabel
import DCLabel.TCB
import Data.List (tails)
import Data.Functor ((<$>))
import System.IO.Unsafe

instance Arbitrary Principal where 
     arbitrary = do p <- oneof $ map return ["A", "B", "C"]
                    return $ principal p


instance Arbitrary Disj where 
     arbitrary = sized disjunction 
                 where disjunction 0 = return $ MkDisj { disj = [] }
                       disjunction n = do a  <- arbitrary
                                          m  <- choose (0, n-1) 
                                          djs <- disjunction m
                                          return $ MkDisj $ a:(disj djs)     


instance Arbitrary Conj where 
     arbitrary = sized conjunction 
                 where conjunction 0 = oneof 
                        [ return $ MkConj { conj = [] }
                        , return $ MkConj { conj = [MkDisj []] }
                        , return $ MkConj { conj = [MkDisj [], MkDisj []] }
                        ] 
                       conjunction n = do a  <- arbitrary
                                          m  <- choose (0, n-1) 
                                          cjs <- conjunction m
                                          return $ MkConj $ a:(conj cjs)     
     shrink (MkConj ls) = [MkConj ll | l <- tails ls, ll <- shrink l]

instance Arbitrary Component where
  arbitrary = do m <- choose (0, 1) :: Gen Int
                 if m==0 then mkArbLbl arbitrary
			 else return MkComponentAll
    where mkArbLbl :: Gen Conj -> Gen Component
          mkArbLbl = liftM MkComponent

instance Arbitrary DCLabel where
  arbitrary = do s <- arbitrary
                 i <- arbitrary 
                 return $ MkDCLabel { secrecy = s, integrity = i }

instance Arbitrary TCBPriv where
  arbitrary = do p <- arbitrary
                 return $ MkTCBPriv p
main :: IO ()
main = defaultMain tests

monadicDC :: PropertyM DC a -> Property
monadicDC (MkPropertyM m) =
 property $ unsafePerformIO <$> run <$> m (const (return (return (property True))))
  where run x =fst <$> evalDC x

-- | Check that the current label is raised when unlabeling a labeled value
prop_label_unlabel = monadicDC $ do
  l    <- pick (arbitrary :: Gen DCLabel)
  x    <- pick (arbitrary :: Gen Int)
  lbl0 <- run $ getLabel
  clr  <- run $ getClearance
  pre $ l `leq` lbl0 && l `leq` clr
  lx   <- run $ label l x
  x'   <- run $ unlabel lx
  lbl1 <- run $ getLabel
  assert $ lbl1 == l && x' == x
  

tests :: [Test]
tests = [
    testGroup "label" [
      testProperty "unlabel raises current label" prop_label_unlabel
    ]
  ]
