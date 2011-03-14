module Main where

import LIO.LIO
import LIO.TCB (showTCB)
import LIO.HiStar
import Control.Monad

import Prelude hiding (catch)
import System.IO.Error hiding (catch)
import qualified Data.Map as Map

hLabel = HSL (Map.singleton (HSC 3) L3) L1 :: HSLabel
lLabel = HSL (Map.singleton (HSC 3) L1) L1 :: HSLabel


-- flow below current label
problemTest1 = do
  (r,cL) <- evalHS $ do
    lr <- lref lLabel 5
    hr <- lref hLabel 4
    h <- openR hr
    return $ if (h `mod` 2) == 0 
               then liftM (\v -> 0) lr
               else liftM (\v -> 1) lr
  return (showTCB r,cL)

-- flow above current clearance
problemTest2 = do
  (r,cL) <- evalHS $ do
    hr <- lref hLabel 5
    setClearance lLabel
    let hr' = liftM (+4) hr
    return hr'
  return (showTCB r,cL)




