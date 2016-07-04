module Main where 

import LIO
import LIO.DCLabel
import LIO.Concurrent
import LIO.Concurrent.LChan

import Control.Monad

import LIO.TCB (ioTCB)

main :: IO ()
main = evalDC $ do
  logger <- newLChan ("Logger" %% True)
  forkLIO $ forever $ do
    msg  <- readLChan logger
    lcur <- getLabel
    ioTCB $ putStrLn $ show lcur ++ " > " ++ msg
  forkLIO $ do
    writeLChan logger "in them alternate threads"
  forkLIO $ do
    taint ("Alice" %% True)
    writeLChan logger "i has failed"
  forM_ oneToTen $ \i -> writeLChan logger $ "yo "++ show  i
    where oneToTen :: [Int]
          oneToTen = [1..10]
