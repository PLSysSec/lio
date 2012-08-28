
module Main where -- (main) where

import Control.Monad

import LIO
import LIO.Concurrent
import LIO.DCLabel

import System.Clock

cmp :: DCLabeled Int
        -> DCLabeled Int
        -> DC (DCLabeled (Maybe Bool))
cmp la lb = lBracket (lub (labelOf la) (labelOf lb)) (500) $ do
  a <- unlabel la
  b <- unlabel lb
  when (a == b) $ forever (return ())
  return (a == b)

exec :: Int -> Int -> IO ()
exec x y = runDC' $ do
  a <- label (dcLabel (toComponent "alice") dcTrue) x
  b <- label (dcLabel (toComponent "bob") dcTrue) y
  c <- cmp a b
  unlabel c
    where runDC' act = do
            t0 <- getTime Monotonic
            res <- runDC act
            t1 <- getTime Monotonic
            let micro t = (sec t * 1000000) + (nsec t `div` 1000)
            putStrLn $ "Result   = " ++ show res
                    ++ "Duration = " ++ show (micro t1 - micro t0)
