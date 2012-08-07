
module Main (main) where

import LIO
import LIO.DCLabel
import LIO.DCLabel.Instances
import Test.QuickCheck
import Criterion.Main

main = do
  b0 <- mkCanFlowTo 
  b1 <- mkCanFlowToP
  defaultMain $ b0 ++ b1

mkCanFlowTo = do
  xs <- sample' (arbitrary :: Gen DCLabel)
  ys <- sample' (arbitrary :: Gen DCLabel)
  let zs = zipWith (\x y -> whnf (canFlowTo x) y) xs ys
      bs = zipWith (\n b -> bench ("canFlowTo " ++ show n) b) [1..] zs
  return bs

mkCanFlowToP = do
  xs <- sample' (arbitrary :: Gen DCLabel)
  ys <- sample' (arbitrary :: Gen DCLabel)
  ps <- sample' (arbitrary :: Gen DCPriv)
  let zs = zipWith3 (\p x y -> whnf (canFlowToP p x) y) ps xs ys
      bs = zipWith (\n b -> bench ("canFlowToP " ++ show n) b) [1..] zs
  return bs
