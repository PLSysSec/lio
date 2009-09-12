
module Main (-- module LIO.LIO
              module LIO.TCB
            , module LIO.Handle
            , module LIO.DCLabel
            , module LIO.FS
            , module Main
            ) where

-- import LIO.LIO
import LIO.TCB
import LIO.Handle
-- import qualified LIO.Handle as LH
import LIO.DCLabel
import LIO.FS

import Prelude hiding (catch, readFile, writeFile)
-- import Control.Exception hiding (throwIO, catch)
import Control.Applicative
import qualified Data.ByteString.Lazy as L
-- import Data.Set (Set)
import qualified Data.Set as Set
import System.IO hiding (readFile, writeFile)
import System.IO.Error hiding (catch)

cat1 :: DCat t
cat1 = DCat (Set.fromList [Principal "my@address.com"
                          , Principal "your@address.com"])
cat2 :: DCat t
cat2 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "your@address.com"])
cat3 :: DCat t
cat3 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "yourother@address.com"])
cat4 :: DCat t
cat4 = DCat (Set.singleton $ Principal "my@example.com")

e :: DCLabel
e = DCLabel (dcsSingleton cat1) (dcsFromList [cat1, cat2])
d :: DCLabel
d = DCLabel (dcsFromList [cat1, cat2]) (dcsFromList [cat1, cat2])
h :: DCLabel
h = DCLabel (dcsEmpty) (dcsFromList [cat1, cat2])

l1 :: DCLabel
l1 = DCLabel (dcsFromList [cat2, cat3]) (dcsFromList [cat3, cat4])
l2 :: DCLabel
l2 = DCLabel (dcsFromList [cat3, cat4]) (dcsFromList [cat2, cat3])

rl :: String -> [(DCLabel, String)]
rl = reads

md :: IO ((), DCLabel)
md = evalDC $ mkDir NoPrivs h rootDir "high"

maybeReadFile :: String -> DC (Maybe L.ByteString)
maybeReadFile path =
  catch (Just <$> readFile path)
            (\ex -> if isDoesNotExistError ex
                   then return Nothing
                   else throwIO ex)

doReadFile :: String -> DC L.ByteString
doReadFile path = readFile path

main :: IO ()
main = return ()
