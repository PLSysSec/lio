{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | Instances for "QuickCheck"\'s 'Arbitrary' class.
module Instances {-()-} where

import Data.Typeable

import Control.Monad
import Control.Monad.Loc
import Control.Exception hiding (onException)

import Test.QuickCheck hiding (label)
import Test.QuickCheck.Instances
import DCLabel.Instances

import LIO
import LIO.TCB (showTCB)
import LIO.Labeled.TCB
import LIO.DCLabel

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

instance Arbitrary DCAction where
  arbitrary = oneof lioActs
