{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Prelude hiding (catch)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck hiding (label)
import Test.QuickCheck.Monadic

import Data.List (tails)
import Data.Functor ((<$>))
import Data.IORef

import LIO.TCB
import LIO.DCLabel
import DCLabel.TCB
import DCLabel.PrettyShow

import Control.Monad (liftM, void, forM_)
import Control.Exception (SomeException(..))
import Control.Concurrent

import System.IO.Unsafe

instance Arbitrary Principal where 
     arbitrary = do p <- oneof $ map return ["A", "B", "C", "D"]
                    return $ principal (p :: String)


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

--
--
--

main :: IO ()
main = defaultMain tests

--
--
--

monadicDC :: PropertyM DC a -> Property
monadicDC (MkPropertyM m) =
 property $ unsafePerformIO <$> dorun <$> m (const (return (return (property True))))
  where dorun x =fst <$> evalDC x

-- Helper function
printLIOState :: String -> DC ()
printLIOState m = do
  s <- getTCB
  ioTCB . putStrLn $ "\n" ++ m ++ ":\nLabel = " ++ (prettyShow $ lioL s ) 
                          ++ "\nClear = " ++ (prettyShow $ lioC s ) 
                          ++ "\nPrivs = " ++ (prettyShow $ lioP s ) 

--
--
--

tests :: [Test]
tests = [
    testGroup "label" [
      testProperty
        "unlabel raises current label"
        prop_label_unlabel
    ]
  , testGroup "withPrivileges" [
      testProperty
        "restores privileges: nop" $
        prop_withPrivileges_restores_privs (return ())
    , testProperty
        "restores privileges: exception" $
        prop_withPrivileges_restores_privs (throwIO . userError $ "ex")
    , testProperty
        "restores privileges: IFC violation" $
        prop_withPrivileges_restores_privs (do taint ltop
                                               void $ label lbot '1')
    , testProperty
        "does not escallate privileges"
        prop_withPrivileges_no_escalate 
    ]
  , testGroup "catch" [
      testProperty
        "does not untaint computation: id" $
        prop_catch_preserves_taint (const id)
    , testProperty
        "does not untaint computation: withPrivileges" $
        prop_catch_preserves_taint withPrivileges
    , testProperty
        "does not untaint computation: toLabeled" $
        prop_catch_preserves_taint withPrivileges
    ]
  , testGroup "onException" [
      testProperty
        "does not untaint computation: id" $
        prop_onException_preserves_taint (const id)
    , testProperty
        "does not untaint computation: withPrivileges" $
        prop_onException_preserves_taint withPrivileges
    , testProperty
        "does not untaint computation: toLabeled" $
        prop_onException_preserves_taint withPrivileges
    ]
  , testGroup "mask" [
      testProperty
        "does not untaint computation: id" $
        prop_mask_preserves_taint (const id)
    , testProperty
        "does not untaint computation: withPrivileges" $
        prop_mask_preserves_taint withPrivileges
    , testProperty
        "does not untaint computation: toLabeled" $
        prop_mask_preserves_taint withPrivileges
    , testProperty
        "restore allows throwTo exceptions" $
        prop_mask_correct  True
    , testProperty
        "mask without restore ignores throwTo exceptions" $
        prop_mask_correct  False
    ]
  ]

--
-- label/unlabe related
-- 

-- NOTE: we assume that the initial label is bottom and initial
-- clearance is top

-- | Check that the current label is raised when unlabeling a labeled value
prop_label_unlabel :: Property
prop_label_unlabel = monadicDC $ do
  l    <- pick (arbitrary :: Gen DCLabel)
  x    <- pick (arbitrary :: Gen Int)
  lx   <- run $ label l x
  x'   <- run $ unlabel lx
  lbl1 <- run $ getLabel
  assert $ lbl1 == l && x' == x
  
--
-- withPrivileges related
-- 

-- | Makesure that the privileges after a withPrivileges are restored
-- correctly
prop_withPrivileges_restores_privs :: DC () -> Property 
prop_withPrivileges_restores_privs act = monadicDC $ do
  p0 <- pick (arbitrary :: Gen TCBPriv)
  p1 <- pick (arbitrary :: Gen TCBPriv)
  pre $ p0 /= p1
  run $ setPrivileges p0
  run $ (withPrivileges p1 act) `catchTCB` (\_ -> return ())
  p0' <- run $ getPrivileges
  assert $ p0 == p0'


-- | Assert that the privileges in a withPrivileged block are only
-- what is provided.
prop_withPrivileges_no_escalate :: Property 
prop_withPrivileges_no_escalate = monadicDC $ do
  p0 <- pick (arbitrary :: Gen TCBPriv)
  p1 <- pick (arbitrary :: Gen TCBPriv)
  pre $ p0 /= p1
  run $ setPrivileges p0
  p1' <- run $ withPrivileges p1 getPrivileges
  assert $ p1 == p1'

--
-- catch related
-- 

-- | Taint within catch does not get ignored
prop_catch_preserves_taint :: (TCBPriv -> DC DCLabel -> DC DCLabel) -> Property 
prop_catch_preserves_taint wrapper = monadicDC $ do
  p <- pick (arbitrary :: Gen TCBPriv)
  l  <- pick (arbitrary :: Gen DCLabel)
  l' <- run $ catch (wrapper p $ do taint l
                                    _ <- throwIO (userError "u")
                                    getLabel
                    ) (\(SomeException _) -> getLabel)
  l'' <- run $ getLabel
  assert $ l == l' && l == l''

--
-- onException related
-- 

-- | onException does not ignore taint
prop_onException_preserves_taint :: (TCBPriv -> DC DCLabel -> DC DCLabel)
                                 -> Property 
prop_onException_preserves_taint wrapper = monadicDC $ do
  p <- pick (arbitrary :: Gen TCBPriv)
  l  <- pick (arbitrary :: Gen DCLabel)
  l' <- run $ catch ((wrapper p $ do taint l
                                     _ <- throwIO (userError "u")
                                     getLabel
                     ) `onException`
                         (return ())) (\(SomeException _) -> getLabel)
  l'' <- run $ getLabel
  assert $ l == l' && l == l''

--
-- mask related
-- 

-- | onException does not ignore taint
prop_mask_preserves_taint :: (TCBPriv -> DC DCLabel -> DC DCLabel) -> Property 
prop_mask_preserves_taint wrapper = monadicDC $ do
  p  <- pick (arbitrary :: Gen TCBPriv)
  l  <- pick (arbitrary :: Gen DCLabel)
  doRestore <- pick (arbitrary :: Gen Bool)
  l' <- run $ catch (mask $ \restore -> (if doRestore then restore else id) $
                       wrapper p $ do taint l
                                      _ <- throwIO (userError "u")
                                      getLabel
                    ) (\(SomeException _) -> getLabel)
  l'' <- run $ getLabel
  assert $ l == l' && l == l''


-- | Check that if restore is used then a throwTo is not ignored.
-- Conversely, if restore is not used then a throwTo should not affect
-- the count.
prop_mask_correct :: Bool -> Property
prop_mask_correct doRestore = monadicIO $ do
  let count = 1000000
  ref <- run $ newIORef 1
  run $ do
    tid <- forkIO $ (\(SomeException _) -> return ()) `handle` (void $ evalDC $ 
        mask $ \restore -> (if doRestore then restore else id) $ do
          forM_ [1..count] (ioTCB . writeIORef ref))
    threadDelay 10
    throwTo tid (userError "kill")
  res <- run $ readIORef ref
  assert $ if doRestore then res < count else res == count
