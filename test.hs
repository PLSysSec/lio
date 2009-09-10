
module Main (-- module LIO.LIO
             module LIO.TCB
            , module LIO.DCLabel
            , module LIO.FS
            , module LIO.Handle
            , module Main
            ) where

import LIO.LIO
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

cat1 = DCat (Set.fromList [Principal "my@address.com"
                          , Principal "your@address.com"])
cat2 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "your@address.com"])
cat3 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "yourother@address.com"])
cat4 = DCat (Set.singleton $ Principal "my@example.com")

e = DCLabel (dcsSingleton cat1) (dcsFromList [cat1, cat2])
d = DCLabel (dcsFromList [cat1, cat2]) (dcsFromList [cat1, cat2])
h = DCLabel (dcsEmpty) (dcsFromList [cat1, cat2])

l1 = DCLabel (dcsFromList [cat2, cat3]) (dcsFromList [cat3, cat4])
l2 = DCLabel (dcsFromList [cat3, cat4]) (dcsFromList [cat2, cat3])

rl :: String -> [(DCLabel, String)]
rl = reads

md = evalDC $ mkDir NoPrivs h rootDir "high"

maybeReadFile :: String -> DC (Maybe L.ByteString)
maybeReadFile path =
  catch (Just <$> readFile path)
            (\e -> if isDoesNotExistError e
                   then return Nothing
                   else throwIO e)

doReadFile :: String -> DC L.ByteString
doReadFile path = readFile path

main :: IO ()
main = return ()
