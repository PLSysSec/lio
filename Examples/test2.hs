module Main where

import LIO.LIO
import LIO.HiStar
import Control.Monad

import qualified Data.Map as Map

hLabel = HSL (Map.singleton (HSC 3) L3) L1 :: HSLabel
mLabel = HSL (Map.singleton (HSC 3) L2) L2 :: HSLabel
lLabel = HSL (Map.singleton (HSC 3) L1) L1 :: HSLabel

main = evalHS $ do
  h <- lref hLabel 32
  l <- lref lLabel 42
  hD <- lrefD hLabel 32
  lD <- lrefD lLabel 42
  setClearance mLabel
  sequence [labelOfR l, labelOfR h, Just `liftM` labelOfRD lD, Just `liftM` labelOfRD hD]
  
