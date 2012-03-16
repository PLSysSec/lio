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
      t <- toLabeledThunk $ do
        b <- unlabel lb
        if b == 5
          then throwIO (userError "some exception")
          else return ()
      unlabel t
      return ()
     ) (\(e::SomeException) -> return ())
    return ()
  print . pShow $ l
