{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
import safe Control.Monad
import safe Prelude hiding (writeFile, readFile)
import safe LIO
import safe LIO.DCLabel
import safe LIO.FS.Simple
import safe LIO.FS.Simple.DCLabel
import safe qualified Data.ByteString.Char8 as S8
import LIO.TCB



alice = PrivTCB  $ toCNF "alice"
bob   = PrivTCB  $ toCNF "bob"

main = withDCFS "/tmp/fslio/root" $ tryDC $ do
  putStrLnTCB "ALICE:"
  aliceCode
  putStrLnTCB "\nBOB:"
  bobCode
  putStrLnTCB "\nALICE:"
  aliceCode2

aliceCode = withClearance (alice %% cTrue) $ do
  -- directory for alice and bob
  createDirectoryP alice (alice \/ bob %% alice) "/ab"
    `catch` (\(e::SomeException) -> putStrLnTCB $ " ignorin error: "++ show e)

  -- alice's secret:
  writeFileP alice (Just $ alice %% alice) "/ab/alice" $ secretContent

  -- alice's message to bob:
  writeFileP alice (Just $ alice \/ bob %% alice \/ bob) "/ab/bob" $ abContent

  -- Check to make sure that we wrote the right things:
  lsecret <- labelOfFileP alice "/ab/alice"
  putStrLnTCB $ " secret label is ok? " ++ show (lsecret == alice %% alice)

  secretContent' <- readFileP alice "/ab/alice"
  putStrLnTCB $ " secret content is ok? " ++ show (secretContent' == secretContent)

  abContent' <- readFileP alice "/ab/bob"
  putStrLnTCB $ " bob content is ok? " ++ show (abContent' == abContent)

  where secretContent = S8.pack "w00t w00t"
        abContent = S8.pack "411(3 was here"

bobCode = withClearance (bob %% cTrue) $ do
  -- Try to read and write to everything in the /ab directory
  files <- getDirectoryContentsP bob "/ab"
  let files' = filter (\x -> x `notElem` [".", ".."]) files
  forM_ files' $ \file -> do
    lfile <- labelOfFileP bob $ "/ab/"++file
    cfile <- (S8.unpack `liftM` readFileP bob ("/ab/"++file))
              `catch` (\(e::SomeException) -> return $ "Failed to read: "++ show e)
    (appendFileP bob ("/ab/"++file) baContent)
              `catch` (\(e::SomeException) -> putStrLnTCB $ "Failed to write: "++ show e)
    putStrLnTCB $ " label: " ++ show lfile
    putStrLnTCB $ " text: " ++ cfile

  where baContent = S8.pack "\nb0b was here"


aliceCode2 = withClearance (alice %% cTrue) $ do
  -- Read message from bob and remove all files and containing directory

  abContent <- readFileP alice "/ab/bob"
  putStrLnTCB $ " from bob: " ++ show abContent

  files <- getDirectoryContentsP alice "/ab"
  let files' = filter (\x -> x `notElem` [".", ".."]) files
  forM_ files' $ \file -> do
    putStrLnTCB $ " removing " ++ file
    removeFileP alice $ "/ab/" ++ file
  removeDirectoryP alice "/ab"
  putStrLnTCB $ "Remaining files: "
  files <- getDirectoryContentsP alice "/"
  forM_ files' $ \file ->
    putStrLnTCB $ " " ++ file

putStrLnTCB :: String -> DC ()
putStrLnTCB = ioTCB . putStrLn
