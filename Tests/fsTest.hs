{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Exception as E
import System.Posix.Unistd
import System.Process
import System.FilePath
import Data.Functor
import Data.Serialize
import LIO.TCB (ioTCB)
import LIO
import LIO.DCLabel hiding(Label)
import LIO.Handle
import LIO.FS
import DCLabel.PrettyShow
import DCLabel.Core (createPrivTCB)
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

instance Serialize DCLabel where
  put = put . show
  get = read <$> get

dcEvalWithRoot :: FilePath -> DC a ->  IO (a, DCLabel)
dcEvalWithRoot path act = evalWithRoot path act ()

main = do
--  system "rm -rf /tmp/rootFS"
  (_,l) <-  dcEvalWithRoot "/tmp/rootFS" $ do
{-
    ioTCB $ do
      createDirectoryTCB (newDC "alice" "alice") "alice"
      createDirectoryTCB (newDC "bob"   (<>)) "bob"
      createDirectoryTCB (newDC "wtf"   (<>)) ("bob" </> "wtf")
      createDirectoryTCB (newDC "crap"  (<>)) ("bob" </> "wtf" </> "crap")
      createDirectoryTCB (newDC "hiho"  (<>)) ("bob" </> "wtf" </> "crap" </> "wee")
      do h <- createFileTCB (newDC "leet"  (<>)) ("bob" </> "wtf" </> "leet") WriteMode
         hPutStrLn h (LC.pack "w00t")
         hClose h
      do h <- createFileTCB (newDC "neat"  (<>)) ("neat") WriteMode
         hPutStrLn h (LC.pack "n347")
         hClose h
-}
    --(lookupObjPathP NoPrivs "/bob/nothere" >> return () )`catch` (\(_::E.SomeException) -> return ())
    --getDirectoryContents "bob/" >>= \d -> ioTCB $ mapM_ IO.putStrLn d

    printCurLabel "A"
    l <- getLabel
    h <- openFileP (createPrivTCB $ newPriv ("bob" ./\. "wtf"))
                   (Just $ newDC "leet2"  (<>))
                   ("bob" </> "wtf" </> "leet4") AppendMode
    printCurLabel "B"
    hPutStrLn h (LC.pack "w88t")
    hClose h
{-
    h <- openFileP (createPrivTCB $ newPriv ("bob" ./\. "wtf"))
                   Nothing --(newDC "leet2" (<>))
                   "bob/wtf/leet"
                   ReadMode
    printCurLabel "B"
    hGetContents h >>= \c -> ioTCB $ LC.hPutStrLn IO.stdout (c :: L.ByteString)
-}
    printCurLabel "C"
    {-
    getDirectoryContents "alice" >>= \d -> ioTCB $ mapM_ IO.putStrLn d
    ioTCB $ putStrLn "----"
    createDirectory  "alice/wink"
    ioTCB $ putStrLn "----"
    getDirectoryContents "bob" >>= \d -> ioTCB $ mapM_ IO.putStrLn d
    ioTCB $ putStrLn "----"
    createDirectoryP (createPrivTCB $ newPriv ("bob" ./\. "alice"))
                     (newDC ("alice" ./\. "bob") ("alice"))
                     "alice/crazyie"
    getDirectoryContents "alice/" >>= \d -> ioTCB $ mapM_ IO.putStrLn d
    -}
    return ()
  putStrLn . prettyShow $ l

printCurLabel s = do l <- getLabel 
                     ioTCB . putStrLn $ s ++ ": " ++ (prettyShow l)

--


