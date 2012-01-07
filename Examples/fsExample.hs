{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (catch)
import System.FilePath
import Data.Functor ((<$>))
import Data.Serialize


import Control.Exception (SomeException)
import LIO
import LIO.MonadCatch
import LIO.TCB (ioTCB, setLabelTCB, lowerClrTCB)
import LIO.DCLabel
import LIO.Handle
import DCLabel.PrettyShow
import DCLabel.Core (createPrivTCB)

import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

instance Serialize DCLabel where
  put = put . show
  get = read <$> get

lpub :: DCLabel
lpub = newDC (<>) (<>)

dcEvalWithRoot :: FilePath -> DC a ->  IO (a, DCLabel)
dcEvalWithRoot path act = evalWithRoot path (Just lpub) act ()


-- Enable malicious code:
malicious :: Bool
malicious = False

--
-- Labels and privileges
--

lalice, lbob, lbobOralice :: DCLabel
lalice      = newDC "alice" "alice"
lbob        = newDC "bob" "bob"
lclarice    = newDC "clarice" "clarice"
lbobOralice = newDC ("bob" .\/. "alice") ("bob" .\/. "alice")

palice, pbob, pclarice :: DCPrivTCB
pbob     = createPrivTCB $ newPriv "bob"
palice   = createPrivTCB $ newPriv "alice"
pclarice = createPrivTCB $ newPriv "clarice"


main = ignore $ dcEvalWithRoot "/tmp/rootFS" $ do
  exec bobsCode pbob (newDC "bob" (<>)) "bob"
  exec alicesCode palice (newDC "alice" (<>)) "alice"
  exec claricesCode pclarice (newDC "clairce" (<>)) "clarice"
    where exec act p l s = do setLabelTCB lbot
                              lowerClrTCB ltop
                              putStrLnTCB $ ">Executing for " ++ s ++ ":"
                              catch (withClearance l $ setLabelTCB lpub >>
                                                       act p)
                                    (\(e::SomeException) ->
                                      putStrLnTCB "IFC violation attempt!")
                              printCurLabel $ ">" ++ s
          ignore act = act >> return ()


printCurLabel s = do l <- getLabel 
                     c <- getClearance
                     ioTCB . putStrLn $ s ++ " : " ++ (prettyShow l)
                                          ++ " : " ++ (prettyShow c)

--
-- Bob's code:
--

bobsCode :: DCPrivTCB -> DC ()
bobsCode p = do
  discard_ $ createDirectoryP p lbobOralice "bobOralice"
  discard_ $ createDirectoryP p lbob ("bobOralice" </> "bob")
  -- Write alice a message:
  h <- openFileP p (Just lbobOralice) ("bobOralice" </> "messages") AppendMode
  hPutStrLnP p h (LC.pack "Hi Alice!")
  hCloseP p h
  -- Write secret:
  taint lbob -- writeFileP uses current label to label file
  writeFileP p ("bobOralice" </> "bob" </> "secret")
               (LC.pack "I am Chuck Norris!")

--
-- Alice's code:
-- 
alicesCode :: DCPrivTCB -> DC ()
alicesCode p = do
  when malicious malCode 
  discard_ $ createDirectoryP p lbobOralice "bobOralice"
  discard_ $ createDirectoryP p lalice ("bobOralice" </> "alice")
  files <- getDirectoryContentsP p "bobOralice"
  if "messages" `elem` files
    then do msg <- readFileP p ("bobOralice" </> "messages")
            putStrLnTCB $ "Message log:\n" ++ (LC.unpack msg)
    else writeFileLP p lbobOralice ("bobOralice" </> "messages")
                                   (LC.pack "Hello Bob!")
    where malCode = do
            msg <- readFileP p ("bobOralice" </> "bob" </> "secret")
            putStrLnTCB $ "Bob's secret:\n" ++ (LC.unpack msg)
            


--
-- Clarice's malicious code:
--

claricesCode :: DCPrivTCB -> DC ()
claricesCode p = when malicious $ do
  files <- getDirectoryContentsP p "bobOralice"
  forM_ files $ \f -> putStrLnTCB f


--
-- Misc helper functions
-- 

discard_ :: DC a -> DC ()
discard_ act = do
  c <- getClearance
  catch (discard c act) (\(e::SomeException) -> return ())


putStrLnTCB :: String -> DC ()
putStrLnTCB s = ioTCB $ putStrLn s
