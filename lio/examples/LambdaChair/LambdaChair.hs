{-# LANGUAGE Trustworthy #-}
{- | 

Safe version of "LambdaChair.TCB".

-}

module LambdaChair ( 
    findPaper
  , retrievePaper, readPaper
  , retrieveReview, readReview
  , appendToReview
  , reviewDCPutStrLn
  ) where
import LambdaChair.TCB
