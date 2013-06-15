{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | Instances for "QuickCheck"\'s 'Arbitrary' class.
module LIO.Instances (
    A(..), B(..), C(..), D(..), DCAction(..)
  , DoNotThrow(..)
  ) where

import Data.Typeable
import Data.IORef
import Control.Concurrent.MVar

import Test.QuickCheck hiding (label)
import Test.QuickCheck.Instances ()
import LIO.DCLabel.Instances ()

import LIO
import LIO.LIORef
import LIO.LIORef.TCB (newLIORefTCB, unlabelLIORefTCB)
import LIO.TCB (showTCB)
import LIO.Labeled.TCB
import LIO.DCLabel
import LIO.Concurrent.LMVar
import LIO.Concurrent.LMVar.TCB (newLMVarTCB, unlabelLMVarTCB)
import LIO.Exception

import System.IO.Unsafe

instance (Eq a, Label l) => Eq (Labeled l a) where
  lv1 == lv2 = labelOf lv1 == labelOf lv2 &&
               unlabelTCB lv1 == unlabelTCB lv2

instance (Show a, Label l) => Show (Labeled l a) where
  show = showTCB

instance (Label l, Arbitrary l, Arbitrary a) => Arbitrary (Labeled l a) where
  arbitrary = do
    l <- arbitrary
    x <- arbitrary
    return $ labelTCB l x
    

-- | Exception type
data A = A deriving (Eq, Show, Typeable)
instance Exception A
-- | Exception type
data B = B deriving (Eq, Show, Typeable)
instance Exception B
-- | Exception type
data C = C deriving (Eq, Show, Typeable)
instance Exception C
-- | Exception type
data D = D deriving (Eq, Show, Typeable)
instance Exception D
-- | Exception type that will /never/ be thrown
data DoNotThrow = DoNotThrow deriving (Eq, Show, Typeable)
instance Exception DoNotThrow

-- | General 'DC' action
data DCAction = forall a. (Show a, Arbitrary a) => DCAction (DC a)

instance Show DCAction where
  show = error "No Show instance for DCAction"

instance Arbitrary a => Arbitrary (DCRef a) where
  arbitrary = do
    l <- arbitrary
    a <- arbitrary
    return . unsafePerformIO . evalDC $ newLIORefTCB l a

instance Show a => Show (DCRef a) where
  show lr = let v = unsafePerformIO . readIORef $ unlabelLIORefTCB lr
                l = labelOf lr
            in "DCRef { label = "++ show l ++", value = "++ show v ++" }"

instance Arbitrary a => Arbitrary (LMVar DCLabel a) where
  arbitrary = do
    l <- arbitrary
    a <- arbitrary
    return . unsafePerformIO . evalDC $ newLMVarTCB l a

instance Show a => Show (LMVar DCLabel a) where
  show lr = let v = unsafePerformIO . tryTakeMVar $ unlabelLMVarTCB lr
                l = labelOf lr
            in "LMVar { label = "++ show l ++", value = "++ show v ++" }"


-- random dc actions
lioActs :: [Gen DCAction]
lioActs = [ return . DCAction $ return ()
          , return . DCAction $ getLabel
          , return . DCAction $ getClearance
          , throwAct
          , catchAllAct
          , catchNoneAct
          , onExceptionAct
          , withRandomL guardAlloc
          , withRandomL taint
          , withRandomL guardWrite
          , labelAct
          , unlabelAct
          , newLIORefAct
          , writeLIORefAct
          , readLIORefAct
          , modifyLIORefAct
          , atomicModifyLIORefAct
          ] 
  where throwAct = do
          let ez = [toException A, toException B,toException C,toException D]
          e <- elements ez
          return . DCAction $ (throwLIO e :: DC ())
        catchAllAct = do
          (DCAction act) <- arbitrary
          a <- arbitrary
          return . DCAction $ act `catch` (\(_::SomeException) -> return a)
        catchNoneAct = do
          (DCAction act) <- arbitrary
          a <- arbitrary
          return . DCAction $ act `catch` (\(_::DoNotThrow) -> return a)
        onExceptionAct = do
          (DCAction act1) <- arbitrary
          (DCAction act2) <- arbitrary
          return . DCAction $ act1 `onException` act2
        withRandomL act = do
          l <- arbitrary
          return . DCAction $ act l
        labelAct = do
          i <- arbitrary :: Gen Int
          withRandomL (\l -> label l i)
        unlabelAct = do
          lv <- arbitrary :: Gen (DCLabeled Int)
          return . DCAction $ unlabel lv
        newLIORefAct = do
          i <- arbitrary :: Gen Int
          withRandomL (\l -> newLIORef l i)
        readLIORefAct = do
          lv <- arbitrary :: Gen (DCRef Int)
          return . DCAction $ readLIORef lv
        writeLIORefAct = do
          i <- arbitrary :: Gen Int
          lv <- arbitrary :: Gen (DCRef Int)
          return . DCAction $ writeLIORef lv i
        modifyLIORefAct = do
          lv <- arbitrary :: Gen (DCRef Int)
          return . DCAction $ modifyLIORef lv id
        atomicModifyLIORefAct = do
          lv <- arbitrary :: Gen (DCRef Int)
          return . DCAction $ atomicModifyLIORef lv (\i -> (i,i))

instance Arbitrary DCAction where
  arbitrary = oneof lioActs
