{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (catch)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import           Control.Monad
import           Control.Exception (SomeException)

import           System.FilePath

import           LIO
import           LIO.DCLabel
import           LIO.Concurrent
import           LIO.Handle
import           LIO.TCB (ioTCB, updateLIOStateTCB)
import           LIO.Privs.TCB (mintTCB)


dcEvalWithRoot :: FilePath -> DC a ->  IO a
dcEvalWithRoot path act = evalWithRootFS path (Just dcPub) act defaultState


-- Enable malicious code:
malicious :: Bool
malicious = False

-- Execute alice first?
execAliceFirst :: Bool
execAliceFirst = True

--
-- Labels and privileges
--

lalice, lbob, lbobOralice :: DCLabel
lalice      = dcLabel (toComponent "alice")   (toComponent "alice")
lbob        = dcLabel (toComponent "bob")     (toComponent "bob")
lclarice    = dcLabel (toComponent "clarice") (toComponent "clarice")
lbobOralice = dcLabel ("bob" \/ "alice")      ("bob" \/ "alice")

palice, pbob, pclarice :: DCPriv
pbob     = mintTCB $ dcPrivDesc "bob"
palice   = mintTCB $ dcPrivDesc "alice"
pclarice = mintTCB $ dcPrivDesc "clarice"


main = dcEvalWithRoot "/tmp/lio_fs/root" $ do
  putStrLnTCB $ "malicious = " ++ show malicious
  putStrLnTCB $ "execAliceFirst = " ++ show execAliceFirst
  let execA = exec alicesCode palice (dcLabel (toComponent "alice") dcTrue) "alice"
      execB = exec bobsCode pbob (dcLabel (toComponent "bob") dcTrue) "bob"
  if execAliceFirst 
    then execA >> execB
    else execB >> execA
  exec claricesCode pclarice (dcLabel (toComponent "clarice") dcTrue) "clarice"
    where exec act p l s = do putStrLnTCB $ ">Executing for " ++ s ++ ":"
                              catchLIO (withClearance l $ setLabelTCB dcPub >> act p)
                                       (\(e::SomeException) ->
                                        putStrLnTCB "IFC violation attempt!")
                              printCurLabel $ ">" ++ s
                              putStrLnTCB "\n"
          setLabelTCB l = updateLIOStateTCB $ \s -> s { lioLabel = l }


printCurLabel s = do l <- getLabel 
                     c <- getClearance
                     ioTCB . putStrLn $ s ++ " : " ++ show l ++ " : " ++ show c

--
-- Bob's code:
--

bobsCode :: DCPriv -> DC ()
bobsCode p = do
  discard_ $ createDirectoryP p lbobOralice "bobOralice"
  discard_ $ createDirectoryP p lbob ("bobOralice" </> "bob")
  -- Write alice a message:
  h <- openFileP p (Just lbobOralice) ("bobOralice" </> "messages") AppendMode
  hPutStrLnP p h (LC.pack "Hi Alice!")
  hCloseP p h
  -- Write secret:
  writeFileP p lbob ("bobOralice" </> "bob" </> "secret")
                    (LC.pack "I am Chuck Norris!")


--
-- Alice's code:
-- 

alicesCode :: DCPriv -> DC ()
alicesCode p = do
  when malicious malCode 
  discard_ $ createDirectoryP p lbobOralice "bobOralice"
  discard_ $ createDirectoryP p lalice ("bobOralice" </> "alice")
  files <- getDirectoryContentsP p "bobOralice"
  if "messages" `elem` files
    then do msg <- readFileP p ("bobOralice" </> "messages")
            putStrLnTCB $ "Message log:\n" ++ (LC.unpack msg)
    else writeFileP p lbobOralice ("bobOralice" </> "messages")
                                   (LC.pack "Hello Bob!")
    where malCode = do
            msg <- readFileP p ("bobOralice" </> "bob" </> "secret")
            putStrLnTCB $ "Bob's secret:\n" ++ (LC.unpack msg)
            


--
-- Clarice's malicious code:
--

claricesCode :: DCPriv -> DC ()
claricesCode p = do
  discard_ $ createDirectoryP p lclarice "clarice"
  when malicious $ do
    files <- getDirectoryContentsP p "bobOralice"
    forM_ files $ \f -> putStrLnTCB f


--
-- Misc helper functions
-- 

discard_ :: DC a -> DC ()
discard_ act = do
  forkLIO $ void act
  threadDelay 100


putStrLnTCB :: String -> DC ()
putStrLnTCB s = ioTCB $ putStrLn s



{- OUTPUT:
*Main> main
malicious = False
execAliceFirst = False
>Executing for bob:
>bob : < |True , |True > : < |False , |True >


>Executing for alice:
Message log:
Hi Alice!

>alice : < |True , |True > : < |False , |True >


>Executing for clarice:
>clarice : < |True , |True > : < |False , |True >

- rm -rf /tmp/lio_fs/root -------------------------------------------
*Main> main
malicious = True
execAliceFirst = False
>Executing for bob:
>bob : < |True , |True > : < |False , |True >


>Executing for alice:
IFC violation attempt!
>alice : < |True , |True > : < |False , |True >


>Executing for clarice:
IFC violation attempt!
>clarice : < |True , |True > : < |False , |True >

- rm -rf /tmp/lio_fs/root -------------------------------------------
*Main> main
malicious = False
execAliceFirst = True
>Executing for alice:
>alice : < |True , |True > : < |False , |True >


>Executing for bob:
>bob : < |True , |True > : < |False , |True >


>Executing for clarice:
>clarice : < |True , |True > : < |False , |True >


- rm -rf /tmp/lio_fs/root -------------------------------------------
*Main> main
malicious = True
execAliceFirst = True
>Executing for alice:
IFC violation attempt!
>alice : < |True , |True > : < |False , |True >


>Executing for bob:
>bob : < |True , |True > : < |False , |True >


>Executing for clarice:
IFC violation attempt!
>clarice : < |True , |True > : < |False , |True >


-}

