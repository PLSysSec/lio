{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | Instances for "QuickCheck"\'s 'Arbitrary' class.
module LIO.Instances {-()-} where

import Data.Typeable
import Data.IORef
import Control.Concurrent.MVar

import Control.Monad
import Control.Exception hiding (onException)

import Test.QuickCheck hiding (label)
import Test.QuickCheck.Instances
import LIO.DCLabel.Instances

import LIO
import LIO.LIORef
import LIO.LIORef.TCB (newLIORefTCB, unlabelLIORefTCB)
import LIO.TCB (showTCB)
import LIO.Labeled.TCB
import LIO.DCLabel
import LIO.Concurrent.LMVar
import LIO.Concurrent.LMVar.TCB (newLMVarTCB, unlabelLMVarTCB)

import System.IO.Unsafe

instance (Show a, Label l) => Show (Labeled l a) where
  show = showTCB

instance (Label l, Arbitrary l, Arbitrary a) => Arbitrary (Labeled l a) where
  arbitrary = do
    l <- arbitrary
    x <- arbitrary
    return $ labelTCB l x
    

data A = A deriving (Eq, Show, Typeable)
instance Exception A
data B = B deriving (Eq, Show, Typeable)
instance Exception B
data C = C deriving (Eq, Show, Typeable)
instance Exception C
data D = D deriving (Eq, Show, Typeable)
instance Exception D
data DoNotThrow = DoNotThrow deriving (Eq, Show, Typeable)
instance Exception DoNotThrow

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
          , withRandomL guardAlloc
          , withRandomL taint
          , withRandomL guardWrite
          , labelAct
          , unlabelAct
          , newLIORefAct
          , writeLIORefAct
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
          return . DCAction $ act `catchLIO` (\(_::SomeException) -> return a)
        catchNoneAct = do
          (DCAction act) <- arbitrary
          a <- arbitrary
          return . DCAction $ act `catchLIO` (\(_::DoNotThrow) -> return a)
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
