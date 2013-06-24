{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances
import LIO.DCLabel
import Data.Binary
import Data.Monoid
import Data.Set hiding (map)

import LIO
import LIO.DCLabel.Instances


-- Partial order for DCLabels
prop_dc_porder :: DCLabel -> DCLabel -> Bool
prop_dc_porder l1 l2  = let ge = l1 `canFlowTo` l2
                            le = l2 `canFlowTo` l1
                            eq = l2 == l1
                        in (eq && ge && le) ||  -- ==
                           ((not eq) && (ge || le) && (ge /= le)) || -- < or >
                           (not (eq || ge || le)) -- incomparable

-- L_1 CanFlowTo L_2 ==> L_1 `CanFlowToP p` L_2 for andy p
prop_dc_canFlowToP :: DCLabel -> DCLabel -> Property
prop_dc_canFlowToP l1 l2 = forAll (arbitrary :: Gen DCPriv) $ \p ->
   l1 `canFlowTo` l2 ==> canFlowToP p l1 l2

-- L_1 CanFlowTo_P1 L_2 ==> L_1 `CanFlowToP (P1 /\ P2)` L_2 for andy P2
prop_dc_mappendPrivs :: DCLabel -> DCLabel -> DCPriv -> Property
prop_dc_mappendPrivs l1 l2 p1 = forAll (arbitrary :: Gen DCPriv) $ \p2 ->
   canFlowToP p1 l1 l2 ==> canFlowToP (p1 `mappend` p2) l1 l2

-- Check that labels flow to their join for DCLabels
prop_dc_join :: DCLabel -> DCLabel -> Bool
prop_dc_join l1 l2  = let l3 = l1 `lub` l2
                          t1 = l1 `canFlowTo` l3
                          t2 = l2 `canFlowTo` l3
                      in t1 && t2

-- Check that join is the least upper bound for DCLabels
prop_dc_join_lub ::  DCLabel -> DCLabel -> Property
prop_dc_join_lub l1 l2 = forAll (arbitrary :: Gen DCLabel) $ \l3' ->
 (l1 `canFlowTo` l3') && (l2 `canFlowTo` l3') ==> (l1 `lub` l2) `canFlowTo` l3'
                  

-- Check that meet flows to the labels making it, for DCLabels
prop_dc_meet ::  DCLabel -> DCLabel -> Bool
prop_dc_meet  l1 l2  = let l3 = l1 `glb` l2
                           t1 = l3 `canFlowTo` l1
                           t2 = l3 `canFlowTo` l2
                       in t1 && t2

-- Check that meet the greatest lower bound for DCLabels
prop_dc_meet_glb :: DCLabel -> DCLabel -> Property
prop_dc_meet_glb l1 l2 = forAll (arbitrary :: Gen DCLabel) $ \l3' ->
 (l3' `canFlowTo` l1) && (l3' `canFlowTo` l2) ==> l3' `canFlowTo` (l1 `glb` l2)

main :: IO ()
main = defaultMain tests
--
tests :: [Test]
tests = [
    testGroup "DCLabels" [
      testProperty "Join operation"                             prop_dc_join
    , testProperty "Join operation is the least upper bound"    prop_dc_join_lub
    , testProperty "Meet operation"                             prop_dc_meet
    , testProperty "Meet operation is the greatest lower bound" prop_dc_meet_glb
    , testProperty "DC labels form a partial order"             prop_dc_porder
    , testProperty "Flow check with privs is less restricting"  prop_dc_canFlowToP 
    , testProperty "Combined privileges are stronger"           prop_dc_mappendPrivs
    ]
  ]

