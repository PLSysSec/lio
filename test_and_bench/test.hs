-- {-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main {-(main) -} where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as Q
import Test.HUnit hiding (Test)
import qualified Test.HUnit as HU

import DCLabel hiding (canFlowTo)
import LIO
import LIO.DCLabel
import LIO.Monad.TCB
import LIO.Exception.TCB

import Data.Set hiding (map)
import Data.Serialize
import Data.Typeable

import Control.Monad hiding (join)
import Control.Monad.Loc
import Control.Exception

import DCLabel.Instances -- DCLabel instances
import Instances -- LIO instances

import System.IO.Unsafe

monadicDC :: PropertyM DC a -> Property
monadicDC (MkPropertyM m) =
 property $ unsafePerformIO `liftM` evalDC `liftM` m f
  where f = const . return . return .  property $ True




main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "General" [
         testProperty "Current label always flows to clearance" 
                       prop_guard_curLabel_flowsTo_curClearance 
    ]
  , testGroup "Exceptions" [
        testProperty "catchTCB catches all exceptions"
                     prop_catchTCB_catchAll
      , testProperty "cathLIO catches exception"
                     prop_catchLIO_catchAll 
      , testCase     "catchLIO respects matching types"
                     test_throwA_catchLIOA 
      , testCase     "catchLIO respects SomeException"
                     test_throwA_catchLIOSomeException
      , testCase     "catchLIO respects mis-matching types"
                     test_throwB_catchLIOA 
      , testProperty "catchLIO does not untaint computation"
                     prop_catch_preserves_taint
  ]
  , testGroup "Guards" [
        testProperty "Alloc guard fails if argument above clerance" $
                     prop_guard_fail_if_label_above_clearance guardAlloc
      , testProperty "Taint guard fails if argument above clerance" $
                     prop_guard_fail_if_label_above_clearance taint
      , testProperty "Write guard fails if argument above clerance" $
                     prop_guard_fail_if_label_above_clearance guardWrite
      , testProperty "Read/Write guard fails if argument above clerance" $
                     prop_guard_fail_if_label_above_clearance guardReadWrite
      , testProperty "Alloc guard fails if argument not above current label" $
                     prop_guard_fail_if_label_below_current guardAlloc
      , testProperty "Write guard fails if argument not above current label" $
                     prop_guard_fail_if_label_below_current guardWrite
      , testProperty "Read/Write guard fails if argument not above current label" $
                     prop_guard_fail_if_label_below_current guardReadWrite
      , testProperty "Taint raises current label" $
                     prop_guard_raises_label taint
      , testProperty "Read/write guard raises current label" $
                     prop_guard_raises_label guardReadWrite
    ]
  ]


-- | Current label always flows to current clearance
prop_guard_curLabel_flowsTo_curClearance :: Property
prop_guard_curLabel_flowsTo_curClearance = monadicDC $ do
  (DCAction act) <- pick arbitrary
  a  <- pick arbitrary
  l1 <- run getLabel
  c1 <- run getClearance
  pre $ l1 `canFlowTo` c1
  run $ act `catchLIO` (\(_ :: SomeException) -> return a)
  l2 <- run getLabel
  c2 <- run getClearance
  Q.assert $ l2 `canFlowTo` c2

--
-- Exceptions
--

-- | catchTCB catches all exceptions
prop_catchTCB_catchAll :: Property
prop_catchTCB_catchAll = monadicDC $ do
  (DCAction act) <- pick arbitrary
  a  <- pick arbitrary
  run $ act `catchTCB` (const $ return a)
  Q.assert True

-- | catchLIO catches all exceptions
prop_catchLIO_catchAll :: Property
prop_catchLIO_catchAll = monadicDC $ do
  (DCAction act) <- pick arbitrary
  a  <- pick arbitrary
  run $ act `catchLIO` (\(_ :: SomeException) -> return a)
  Q.assert True

-- | throw and catch type correctness
test_throwA_catchLIOA :: Assertion
test_throwA_catchLIOA = do
  evalDC $ throwLIO A `catchLIO`(\(_ :: A) -> return ())

-- | throw A and catch B should fail
test_throwB_catchLIOA :: Assertion
test_throwB_catchLIOA = do
  evalDC $ (throwLIO B `catchLIO`(\(_ :: A) -> return ())) `catchTCB`
            (\(LabeledExceptionTCB _ _ se) -> case fromException se of
                                                (Just B) -> return ()
                                                _ -> ioTCB $ assertFailure "mismatch")

-- | throw and catch type correctness
test_throwA_catchLIOSomeException :: Assertion
test_throwA_catchLIOSomeException = do
  evalDC $ throwLIO A `catchLIO`(\(_ :: SomeException) -> return ())

-- | Taint within catch does not get ignored
prop_catch_preserves_taint :: Property 
prop_catch_preserves_taint = monadicDC $ do
  l  <- pick (arbitrary :: Gen DCLabel)
  l' <- run $ catchLIO (do taint l
                           throwLIO A
                           getLabel
                    ) (\(SomeException _) -> getLabel)
  l'' <- run $ getLabel
  Q.assert $ l == l' && l == l''

--
-- Guards
--

-- | Guards throw exception if provided label is above clearance
prop_guard_fail_if_label_above_clearance :: (DCLabel -> DC ()) -> Property
prop_guard_fail_if_label_above_clearance act = monadicDC $ 
  forAllM arbitrary $ \ldata -> do
    newc  <- pick arbitrary
    l     <- run getLabel
    pre $ l `canFlowTo` newc
    -- reset clearance from top:
    run $ updateLIOStateTCB $ \s -> s { lioClearance = newc }
    pre . not $ ldata `canFlowTo` newc
    res <- run $ ( act ldata >> return False) `catchTCB` (\_ -> return True)
    Q.assert res

-- | Some guards throw exception if provided label is not above current label.
prop_guard_fail_if_label_below_current :: (DCLabel -> DC ()) -> Property
prop_guard_fail_if_label_below_current act = monadicDC $ 
  forAllM arbitrary $ \ldata -> do
    newl <- pick arbitrary
    c    <- run getClearance
    pre $ newl `canFlowTo` c
    -- reset current label from top:
    run $ updateLIOStateTCB $ \s -> s { lioLabel = newl }
    pre $ (not $ newl `canFlowTo` ldata) && ldata `canFlowTo` c
    res <- run $ ( act ldata >> return False) `catchTCB` (\_ -> return True)
    Q.assert res

-- | Taint raises current label
prop_guard_raises_label :: (DCLabel -> DC ()) -> Property
prop_guard_raises_label act = monadicDC $ do
  ldata <- pick arbitrary
  l1    <- run getLabel
  c     <- run getClearance
  let lres = ldata `join` l1
  pre $ lres `canFlowTo` c
  run $ act lres
  l2    <- run getLabel
  Q.assert $ l2 == lres
