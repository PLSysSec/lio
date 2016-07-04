{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude
import LIO
import LIO.TCB (ioTCB)
import LIO.DCLabel
import LIO.Concurrent


l,m :: DCLabel
l = "A" \/ "B" %% True
m = "M" %% True

main :: IO ()
main =  do
  lr <- evalDC $ do
    lb <- label m (6 :: Int)
    f <- lFork (if doFail then l else m) $ do
      v <- unlabel lb
      return (3+v)
    catch (do r <- lWait f
              ioTCB . putStrLn $ "No exception: " ++ show r 
          ) (\(_::SomeException) -> ioTCB . putStrLn $ "Exception")
  print lr
    where doFail = not True
