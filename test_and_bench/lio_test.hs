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
import LIO.DCLabel.Privs.TCB (allPrivTCB)
import LIO.LIORef
import LIO.LIORef.TCB
import LIO.Privs.TCB (mintTCB)
import LIO.TCB

import Data.Set hiding (map)
import Data.Typeable

import Control.Concurrent hiding (threadDelay)
import Control.Monad
import Control.Exception hiding ( throwIO
                                , catch 
                                , finally
                                , onException
                                , bracket)

import LIO.DCLabel.Instances -- DCLabel instances
import LIO.Instances -- LIO instances
import LIO.Concurrent
import LIO.Concurrent.LMVar
import LIO.Concurrent.LMVar.TCB

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
      , testCase "paranoidDC catches exceptions thrown by 'throw'"
                 test_paranoidDC_catch_throw 
      , testCase "paranoidDC catches 'undefined'"
                 test_paranoidDC_catch_undefined
    ]
  , testGroup "Labeled" [
        testProperty "unlabel raises current label" $
                      prop_gen_raises_label label $ Left (liftJust unlabel)
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
        testProperty "readLIORef raises current label" $
                      prop_gen_raises_label newLIORef $ Left (liftJust readLIORef)
      , testProperty "atomicModifyIORef raises current label" $
                      prop_gen_raises_label newLIORef $
                        Right (\r v -> void $ atomicModifyLIORef r (const (v, v)))

      , testProperty "newLIORef fails if label is not above current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> void $ newLIORef l ()
      , testProperty "writeLIORef fails if label of ref is below current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> newLIORefTCB l () >>= \v -> void $ writeLIORef v ()
      , testProperty "modifyLIORef fails if label of ref is below current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> newLIORefTCB l () >>= \v -> void $ modifyLIORef v id
      , testProperty "atomicModifyLIORef fails if label of ref is below current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> newLIORefTCB l () >>= \v -> void $ atomicModifyLIORef v (const ((), ()))

      , testProperty "newLIORef fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> void $ newLIORef l ()
      , testProperty "writeLIORef fails if label of ref is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newLIORefTCB l () >>= \v -> void $ writeLIORef v ()
      , testProperty "modifyLIORef fails if label of ref is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newLIORefTCB l () >>= \v -> void $ modifyLIORef v id
      , testProperty "atomicModifyLIORef fails if label of ref is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newLIORefTCB l () >>= \v -> void $ atomicModifyLIORef v (const ((), ()))
    ]
  , testGroup "LMVar" [
        testProperty "takeLMVar raises current label" $
                      prop_gen_raises_label newLMVar (Left $ liftJust takeLMVar)
      , testProperty "tryTakeLMVar raises current label" $
                      prop_gen_raises_label newLMVar (Left tryTakeLMVar)
      , testProperty "putLMVar raises current label" $
                      prop_gen_raises_label (\l _-> newEmptyLMVar l) $ Right putLMVar
      , testProperty "readLMVar raises current label" $
                      prop_gen_raises_label newLMVar (Left $ liftJust readLMVar)

      , testProperty "newLMVar fails if label is not above current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> void $ newLMVar l ()
      , testProperty "newEmptyLMVar fails if label is not above current label" $
                     prop_guard_fail_if_label_below_current $
                     \l -> void $ newEmptyLMVar l
      , testProperty "takeLMVar fails if label is below current label" $ 
                     prop_guard_fail_if_label_below_current $
                     \l -> newLMVarTCB l () >>= \v -> void $ takeLMVar v
      , testProperty "readLMVar fails if label is below current label" $ 
                     prop_guard_fail_if_label_below_current $
                     \l -> newLMVarTCB l () >>= \v -> void $ readLMVar v
      , testProperty "putLMVar fails if label is below current label" $ 
                     prop_guard_fail_if_label_below_current $
                     \l -> newEmptyLMVarTCB l >>= \v -> void $ putLMVar v ()

      , testProperty "newEmptyLMVar fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> void $ newEmptyLMVar l
      , testProperty "newLMVar fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> void $ newLMVar l ()
      , testProperty "takeLMVar fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newLMVarTCB l () >>= \v -> void $ takeLMVar v
      , testProperty "tryTakeLMVar fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newLMVarTCB l () >>= \v -> void $ tryTakeLMVar v
      , testProperty "readLMVar fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newLMVarTCB l () >>= \v -> void $ readLMVar v
      , testProperty "putLMVar fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newEmptyLMVarTCB l >>= \v -> void $ putLMVar v ()
      , testProperty "tryPutLMVar fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> newEmptyLMVarTCB l >>= \v -> void $ tryPutLMVar v ()
    ]
  , testGroup "LabeledResult" [
        testProperty "lWait raises current label" $
                      prop_gen_raises_label (\l v -> lFork l (return v))
                                            (Left $ liftJust lWait)
      , testProperty "trylWait raises current label" $
                      prop_gen_raises_label (\l v -> lFork l (return v))
                                            (Left $ \v -> (threadDelay 5000) >> trylWait v)

      , testProperty "lFork fails if label is below current label" $ 
                     prop_guard_fail_if_label_below_current $
                     \l -> void . lFork l $ return ()
      , testProperty "lWait fails if label is below current label" $ 
                     prop_guard_fail_if_label_below_current $
                     \l -> lForkP allPrivTCB l (return ()) >>= \v -> void $ lWait v
      , testProperty "lFork fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> void . lFork l $ return ()
      , testProperty "lWait fails if label is above clearance" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> lForkP allPrivTCB l (return ()) >>= \v -> void $ lWait v
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
  , testGroup "LIO state" [
        testProperty "Trying to set label above clerance fails" $
                     prop_guard_fail_if_label_above_clearance setLabel
      , testProperty "Trying to set label below current label fails" $
                     prop_guard_fail_if_label_below_current   setLabel
      , testProperty "Trying to set clearance above clerance fails" $
                     prop_guard_fail_if_label_above_clearance setClearance
      , testProperty "Trying to set clearance below current label fails" $
                     prop_guard_fail_if_label_below_current setClearance
      , testProperty "withClearance set above clerance fails" $
                     prop_guard_fail_if_label_above_clearance $
                     \l -> withClearance l (return ())
      , testProperty "withClearance set below current label fails" $
                     prop_guard_fail_if_label_below_current $
                     \l -> withClearance l (return ())
  ]
  , testGroup "Gates" [
        testProperty  "callGate correct"
                      callGate_correct
    ]
  , testGroup "Privs" [
        testProperty  "partDowngradeP correct"
                      prop_partDowngradeP_correct 
    ]
  ]
    where liftJust f x = Just `liftM` f x


--
-- General
--

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

-- | paranoidDC catches exceptions thrown by 'throw'
test_paranoidDC_catch_throw :: Assertion
test_paranoidDC_catch_throw = do
  res <- paranoidDC (throw A)
  case res of
    Left se -> HU.assert $ fromException se == Just A
    _ -> assertFailure "did not catch exception"

-- | paranoidDC catches 'undefined'
test_paranoidDC_catch_undefined :: Assertion
test_paranoidDC_catch_undefined = do
  res <- paranoidDC undefined
  case res of
    Left _ -> return ()
    _ -> assertFailure "did not catch exception"

-- | Check that the current label is raised when reading/writing
-- labeled object
prop_gen_raises_label :: (LabelOf t)
                        => (DCLabel -> Int -> DC (t DCLabel Int)) -- constructor
                        -> Either (t DCLabel Int -> DC (Maybe Int)) -- reader
                                  (t DCLabel Int -> Int -> DC ())   -- writer
                        -> Property
prop_gen_raises_label constr rw = monadicDC $ do
  l    <- pick arbitrary
  x    <- pick arbitrary
  lx   <- run $ constr l x
  x'   <- run $ case rw of
                  Left reader -> reader lx
                  Right writer -> writer lx x >> return (Just x) 
  lbl1 <- run $ getLabel
  Q.assert $ lbl1 == l && x' == Just x


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

--
-- partDowngradeP
--

{- | Test partDowngradeP
lr = partDowngradeP p li lg satisfies:
   - canflowto lg lr
   - canflowto_p p li lr
   - lr is the greatest lower bound
-}
prop_partDowngradeP_correct :: DCPriv -> DCLabel -> DCLabel -> Property
prop_partDowngradeP_correct p li lg = 
  let lr = partDowngradeP p li lg 
  in forAll (arbitrary :: Gen DCLabel) $ \lr' -> 
   	canFlowTo lg lr &&
   	canFlowToP p li lr &&
	not ( canFlowTo  lg lr'   &&
              canFlowToP p li lr' &&
	      lr' /= lr &&
	      canFlowTo lr' lr)
