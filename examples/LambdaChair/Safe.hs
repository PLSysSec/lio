{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#endif
module Safe ( module LambdaChair ) where
import LambdaChair ( findPaper
                   , retrievePaper, readPaper
                   , retrieveReview, readReview
                   , appendToReview
                   , reviewDCPutStrLn )
