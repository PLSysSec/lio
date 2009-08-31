
module Main (module LIO.TCB
            , module LIO.FS
            , module LIO.TmpFile
            , module LIO.DCLabel
            , IOMode(..)
            , module Main
            ) where

import LIO.TCB
import LIO.DCLabel
import LIO.FS

import LIO.TmpFile

import Control.Exception
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

cat1 = DCat (Set.fromList [Principal "my@address.com"
                          , Principal "your@address.com"])
cat2 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "your@address.com"])

e = DCLabel (Set.singleton cat1) (Set.fromList [cat1, cat2])
d = DCLabel (Set.fromList [cat1, cat2]) (Set.fromList [cat1, cat2])

rl :: String -> [(DCLabel, String)]
rl = reads


main = return ()
