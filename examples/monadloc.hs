{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
The role of this example is to show the call trace provided by MonadLoc.
-}

import Data.Typeable

import Control.Monad.Loc
import Control.Exception

import LIO
import LIO.DCLabel

data A = A deriving (Eq, Show, Typeable)
instance Exception A

f = do
  return ()
  g `catchLIO` (\(_ :: SomeException) -> return ())
  h

main = evalDC f


g = do
  throwLIO A

h = do
  return ()
  g
