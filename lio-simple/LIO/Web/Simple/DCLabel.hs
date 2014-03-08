{-# LANGUAGE Safe #-}

{- | 

This module exports several types that makes it easier to write LIO
web apps with DCLabels.

-}

module LIO.Web.Simple.DCLabel (
    SimpleDCApplication
  , SimpleDCMiddleware
  ) where

import safe LIO.DCLabel
import safe LIO.Web.Simple

type SimpleDCApplication = SimpleLIOApplication CNF DCLabel
type SimpleDCMiddleware  = SimpleLIOMiddleware CNF DCLabel
