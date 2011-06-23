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

  paper1 <- readPaper p1
  reviewDCPutStrLn $ show paper1

  appendToReview p1 "Interesting work!"

  paper2 <- readPaper p2
  reviewDCPutStrLn $ show paper2
  readReview p2
  appendToReview p2 "What about adding new users?"
  return ()
