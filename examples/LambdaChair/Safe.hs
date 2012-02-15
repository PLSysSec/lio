{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Safe ( module LambdaChair ) where
import LambdaChair ( findPaper
                   , retrievePaper, readPaper
                   , retrieveReview, readReview
                   , appendToReview
                   , reviewDCPutStrLn )
