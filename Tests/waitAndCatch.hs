{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (catch)
import LIO
import LIO.TCB (ioTCB)
import LIO.MonadCatch
import LIO.DCLabel hiding (label)
import LIO.Concurrent
import DCLabel.PrettyShow


l,m,h :: DCLabel
l = lbot
m = newDC "M" (<>)
h = ltop

main =  do
  (_,l) <- evalDC $ do
    lb <- label m 6
    f <- lFork l $ do
      v <- unlabel lb
      return (3+v)
    catch (do r <- lWait f
              ioTCB . print $ r 
          ) (\(e::LabelFault) -> ioTCB . putStrLn $ "exception")
  print . pShow $ l
