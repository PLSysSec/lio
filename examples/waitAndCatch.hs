{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (catch)
import LIO
import LIO.TCB (ioTCB)
import LIO.DCLabel
import LIO.Concurrent
import Control.Exception (SomeException)


l,m,h :: DCLabel
l = dcLabel ("A" \/ "B")      dcTrue
m = dcLabel (toComponent "M") dcTrue
h = dcLabel ("A" /\ "B")      dcTrue

main =  do
  (_,l) <- runDC $ do
    lb <- label m 6
    f <- lFork (if doFail then m else l) $ do
      v <- unlabel lb
      return (3+v)
    catchLIO (do r <- lWait f
                 ioTCB . putStrLn $ "No exception: " ++ show r 
             ) (\(_::SomeException) -> ioTCB . putStrLn $ "Exception")
  print l
    where doFail = True
