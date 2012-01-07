import LIO.TCB
import LIO.DCLabel
import DCLabel.Safe
import Control.Concurrent
import Control.Monad


main = do
  tid <- forkIO $ do
    _ <- evalDC $ 
      mask $ \restore -> do
        forM_ [1..10000] (ioTCB . print)
        ioTCB . putStrLn  $ "bye!"
        restore (return ())
        ioTCB . putStrLn  $ "DONE!"
    return ()
  threadDelay 100000
  throwTo tid (userError "WOO")
  
