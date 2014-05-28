module Main where 

import LIO
import LIO.DCLabel
import LIO.Concurrent
import LIO.Concurrent.LChan

import Control.Monad

import LIO.TCB (ioTCB)

main = evalDC $ do
  log <- newLChan ("Logger" %% True)
  forkLIO $ forever $ do
    msg  <- readLChan log
    lcur <- getLabel
    ioTCB $ putStrLn $ show lcur ++ " > " ++ msg
  forkLIO $ do
    writeLChan log "in them alternate threads"
  forkLIO $ do
    taint ("Alice" %% True)
    writeLChan log "i has failed"
  forM_ [1..10] $ \i -> writeLChan log $ "yo "++ show  i
