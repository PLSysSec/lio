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
import Control.Exception

import Test.QuickCheck
import Test.QuickCheck.Instances
import DCLabel.Instances

import LIO
import LIO.DCLabel

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
          ] 
  where throwAct = do
          let ez = [toException A, toException B,toException C,toException D]
          e <- elements ez
          return . DCAction $ (throwLIO e :: DC ())
        catchAllAct = do
          (DCAction act) <- arbitrary
          a <- arbitrary
          return . DCAction $ act `catchLIO`
                                    (\(_::SomeException) -> return a)
        catchNoneAct = do
          (DCAction act) <- arbitrary
          a <- arbitrary
          return . DCAction $ act `catchLIO`
                                    (\(_::DoNotThrow) -> return a)
        withRandomL act = do
          l <- arbitrary
          return . DCAction $ act l

instance Arbitrary DCAction where
  arbitrary = oneof lioActs
