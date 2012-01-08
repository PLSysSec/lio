{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (catch)
import LIO
import LIO.Concurrent
import LIO.TCB (ioTCB)
import LIO.MonadCatch
import Control.Exception (SomeException)
import LIO.DCLabel
import DCLabel.PrettyShow


l,m,h :: DCLabel
l = lbot
m = newDC "M" (<>)
h = ltop

main =  do
  (_,l) <- evalDC $ do
    lb <- label m 6
    catch ( do
      --r <- lFork ltop $ do
      _ <- toLabeled ltop $ do
        _ <- unlabel lb
        throwIO (userError "label of this exception should be top, not M")
      --lWait r
      return ()
     ) (\(e::SomeException) -> return ())
    return ()
  print . pShow $ l
