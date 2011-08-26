module AliceCode ( mainReview ) where

import Safe

findPaper' s = do
  ep <- findPaper s
  case ep of
    Left e -> error $ "Failed with" ++ e
    Right p -> return p

mainReview = do
  p1 <- findPaper' "Flexible Dynamic"
  p2 <- findPaper' "A Static"

  readPaper p1 >>= \p -> reviewDCPutStrLn $ show p

  appendToReview p1 "Interesting work!"

  readPaper p2 >>= \p -> reviewDCPutStrLn $ show p
  readReview p2 >>= \r -> reviewDCPutStrLn $ show r
  appendToReview p2 "What about adding new users?"
