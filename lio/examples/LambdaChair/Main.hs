{-# LANGUAGE Unsafe #-}

module Main (main) where

import LIO.DCLabel
import LambdaChair.TCB

import safe AliceCode as Alice
import safe BobCode as Bob


main :: IO ()
main = printL . runReviewDC' $ do
  addUser "Alice" "password"
  
  p1 <- addPaper "Flexible Dynamic..."
  p2 <- addPaper "A Static..."
  
  addAssignment "Alice" p1
  addAssignment "Alice" p2
  
  asUser "Alice" $ Alice.mainReview
  
  addUser "Bob" "password"
  
  addAssignment "Bob" p2
  addConflict "Bob" p1

  asUser "Bob" $ Bob.mainReview
    where printL m = m >>= (putStrLn . show . snd)
          runReviewDC' act = 
            runDC $ runReviewDC act (emptyReviewState)
