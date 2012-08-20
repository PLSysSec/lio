{-# LANGUAGE Safe #-}
module BobCode ( mainReview ) where

import LambdaChair

findPaper' s = do
  ep <- findPaper s
  case ep of
    Left e -> error $ "Failed with" ++ e
    Right p -> return p

mainReview = do
  p1 <- findPaper' "Flexible Dynamic..."
  p2 <- findPaper' "A Static..."
  appendToReview p2 "Hmm, IFC.."
  readReview p2
  readReview p1
