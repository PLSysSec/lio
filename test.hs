
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

import Prelude hiding (readFile, writeFile)
import Control.Exception
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO hiding (readFile, writeFile)
import System.IO.Error

cat1 = DCat (Set.fromList [Principal "my@address.com"
                          , Principal "your@address.com"])
cat2 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "your@address.com"])

e = DCLabel (Set.singleton cat1) (Set.fromList [cat1, cat2])
d = DCLabel (Set.fromList [cat1, cat2]) (Set.fromList [cat1, cat2])
h = DCLabel (Set.empty) (Set.fromList [cat1, cat2])

rl :: String -> [(DCLabel, String)]
rl = reads

md = evalDC $ mkDir NoPrivs h rootDir "high"

maybeReadFile :: String -> DC (Maybe L.ByteString)
maybeReadFile path =
  catchL (Just <$> readFile path)
         (\e -> if isDoesNotExistError e
                then return Nothing
                else throwL e)

doReadFile :: String -> DC L.ByteString
doReadFile path = readFile path

main = return ()
