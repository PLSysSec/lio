-- {-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main {-(main) -} where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as Q
import Test.HUnit hiding (Test)
import qualified Test.HUnit as HU

import LIO.DCLabel
import LIO
import LIO.Labeled.TCB (labelTCB)
import LIO.DCLabel
import LIO.LIORef
import LIO.Privs.TCB (mintTCB)
import LIO.TCB

import Data.Set hiding (map)
import Data.Typeable

import Control.Monad
import Control.Exception hiding ( throwIO
                                , catch 
                                , finally
                                , onException
                                , bracket)

import LIO.DCLabel.Instances -- DCLabel instances
import LIO.Instances -- LIO instances

import System.IO.Unsafe

-- | Evaluate LIO computation with starting label bottom
-- and clearance top
doEval :: DC a -> IO a
doEval act = evalLIO act $ LIOState { lioLabel = bottom
                                    , lioClearance = top }

monadicDC :: PropertyM DC a -> Property
monadicDC (MkPropertyM m) =
 property $ unsafePerformIO `liftM` doEval `liftM` m f
  where f = const . return . return .  property $ True




main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "General" [
        testProperty "Current label always flows to clearance" 
                     prop_guard_curLabel_flowsTo_curClearance 
    ]
  , testGroup "Labeled" [
        testProperty "unlabel raises current label"
                      prop_label_unlabel
      , testProperty "label fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> void $ label l ()
      , testProperty "label fails if label is not above current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> void $ label l ()
      , testProperty "unlabel fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> unlabel (labelTCB l ())
    ]
  , testGroup "LIORef" [
        testProperty "readIORef raises current label"
                      prop_newIORef_readIORef
      , testProperty "atomicModifyIORef raises current label"
                      prop_atomicModifyIORef_readIORef
      , testProperty "newLIORef fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> void $ newLIORef l ()
      , testProperty "newLIORef fails if label is not above current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> void $ newLIORef l ()
      , testProperty "writeIORef fails if label of ref is above clearance" $
                     prop_fail_write_ref_above_clearance $
                     \r -> writeLIORef r ()
      , testProperty "writeIORef fails if label of ref is below current label" $
                     prop_fail_write_ref_below_current_label $
                     \r -> writeLIORef r ()
      , testProperty "modifyIORef fails if label of ref is above clearance" $
                     prop_fail_write_ref_above_clearance $
                     \r -> modifyLIORef r id
      , testProperty "modifyORef fails if label of ref is below current label" $
                     prop_fail_write_ref_below_current_label $
                     \r -> modifyLIORef r id
      , testProperty ("atomicModifyORef fails if label of ref is " ++
                      "above clearance") $
                     prop_fail_write_ref_above_clearance  $
                     \r -> atomicModifyLIORef r (\v -> (v, v))
      , testProperty ("atomicModifyORef fails if label of ref is " ++
                      "below current label") $
                     prop_fail_write_ref_below_current_label $
                     \r -> atomicModifyLIORef r (\v -> (v, v))
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
      , testCase     "onException executes final action"
                     test_onException_executes_final_action
      , testCase     "finally executes final action (noException)" $
                     test_finally_executes_final_action (return ())
      , testCase     "finally executes final action (throwLIO A)" $
                     test_finally_executes_final_action (throwLIO A)
      , testCase     "bracket executes final action (noException)" $
                     test_bracket_executes_final_action (return ())
      , testCase     "bracket executes final action (throwLIO A)" $
                     test_bracket_executes_final_action (throwLIO A)
  ]
  , testGroup "Guards" [
        testProperty "Alloc guard fails if argument above clerance" $
                     prop_guard_fail_if_label_above_clearance guardAlloc
      , testProperty "Taint guard fails if argument above clerance" $
                     prop_guard_fail_if_label_above_clearance taint
      , testProperty "Write guard fails if argument above clerance" $
                     prop_guard_fail_if_label_above_clearance guardWrite
      , testProperty "Alloc guard fails if argument not above current label" $
                     prop_guard_fail_if_label_below_current guardAlloc
      , testProperty "Write guard fails if argument not above current label" $
                     prop_guard_fail_if_label_below_current guardWrite
      , testProperty "Taint raises current label" $
                     prop_guard_raises_label taint
    ]
  , testGroup "Gates" [
        testProperty  "callGate correct" $
                      callGate_correct
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
-- Labeled
--

-- | Check that the current label is raised when unlabeling a labeled value
prop_label_unlabel :: Property
prop_label_unlabel = monadicDC $ do
  l    <- pick (arbitrary :: Gen DCLabel)
  x    <- pick (arbitrary :: Gen Int)
  lx   <- run $ label l x
  x'   <- run $ unlabel lx
  lbl1 <- run $ getLabel
  Q.assert $ lbl1 == l && x' == x

--
-- LIORef
--

-- | Check that the current label is raised when reading LIORef
prop_newIORef_readIORef :: Property
prop_newIORef_readIORef = monadicDC $ do
  l    <- pick (arbitrary :: Gen DCLabel)
  x    <- pick (arbitrary :: Gen Int)
  lx   <- run $ newLIORef l x
  x'   <- run $ readLIORef lx
  lbl1 <- run $ getLabel
  Q.assert $ lbl1 == l && x' == x

-- | Check that the current label is raised when atomically modifying LIORef
prop_atomicModifyIORef_readIORef :: Property
prop_atomicModifyIORef_readIORef = monadicDC $ do
  l    <- pick (arbitrary :: Gen DCLabel)
  x    <- pick (arbitrary :: Gen Int)
  lx   <- run $ newLIORef l x
  x'   <- run $ atomicModifyLIORef lx (\v -> (v, v))
  lbl1 <- run $ getLabel
  Q.assert $ lbl1 == l && x' == x

-- | Fail writing/modifying a reference whose label is not below
-- current clearance
prop_fail_write_ref_above_clearance :: (DCRef () -> DC ()) -> Property
prop_fail_write_ref_above_clearance act = monadicDC $ do
  ldata <- pick arbitrary
  newc  <- pick arbitrary
  l     <- run getLabel
  c     <- run getClearance
  pre $ l `canFlowTo` newc
  pre . not $ ldata `canFlowTo` newc
  lref  <- run $ newLIORef ldata ()
  -- reset clearance from top:
  run $ setClearance newc
  res <- run $ ( act lref >> return False) `catchTCB` (\_ -> return True)
  Q.assert res

-- | Fail writing/modifying a reference whose label is not above the
-- current label
prop_fail_write_ref_below_current_label :: (DCRef () -> DC ()) -> Property
prop_fail_write_ref_below_current_label act = monadicDC $ do
  l      <- run getLabel
  c      <- run getClearance
  lref   <- run $ newLIORef l ()
  newl   <- pick arbitrary
  pre $ newl `canFlowTo` c 
  pre . not $ newl `canFlowTo` l -- new label is "above" current label
  run $ taint newl
  res <- run $ ( act lref >> return False) `catchTCB` (\_ -> return True)
  Q.assert res
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
  doEval $ throwLIO A `catchLIO`(\(_ :: A) -> return ())

-- | throw A and catch B should fail
test_throwB_catchLIOA :: Assertion
test_throwB_catchLIOA = do
  doEval $ (throwLIO B `catchLIO`(\(_ :: A) -> return ())) `catchTCB`
            (\(LabeledExceptionTCB _ se) -> case fromException se of
                                              (Just B) -> return ()
                                              _ -> ioTCB $ assertFailure "mismatch")

-- | throw and catch type correctness
test_throwA_catchLIOSomeException :: Assertion
test_throwA_catchLIOSomeException = do
  doEval $ throwLIO A `catchLIO`(\(_ :: SomeException) -> return ())

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

-- | onException executes final action
test_onException_executes_final_action :: Assertion
test_onException_executes_final_action = do
  (res, _) <- tryDC $ throwLIO A `onException` throwLIO B
  case res of
    (Left (LabeledExceptionTCB _ x)) | fromException x == Just B -> return ()
    _ -> assertFailure "should have thrown B"

-- | finally executes final action
test_finally_executes_final_action :: DC () -> Assertion
test_finally_executes_final_action act = do
  (res, _) <- tryDC $ act `finally` throwLIO B
  case res of
    (Left (LabeledExceptionTCB _ x)) | fromException x == Just B -> return ()
    _ -> assertFailure "should have thrown B"

-- | bracket executes final action
test_bracket_executes_final_action :: DC () -> Assertion
test_bracket_executes_final_action act = do
  (res, _) <- tryDC $ bracket (return ()) (const $ throwLIO B) (const act)
  case res of
    (Left (LabeledExceptionTCB _ x)) | fromException x == Just B -> return ()
    _ -> assertFailure "should have thrown B"

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
    -- reset current label from blottom:
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
  let lres = ldata `upperBound` l1
  pre $ lres `canFlowTo` c
  run $ act lres
  l2    <- run getLabel
  Q.assert $ l2 == lres

--
-- Gates
--

-- | Calling gate with right privilege returns True, and False
-- otherwise.
callGate_correct :: Property
callGate_correct = forAll arbitrary $ \(d1 :: DCPrivDesc) ->
                   forAll arbitrary $ \(d2 :: DCPrivDesc) ->
  let p1 = mintTCB d1
      p2 = mintTCB d2
      f = gate $ \d -> if d == privDesc p1 then True else False
  in p1 /= p2 ==> callGate f p1 && (not $ callGate f p2)
