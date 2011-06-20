{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This file exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.
module LIO.Base (
               POrdering(..), POrd(..), o2po, Label(..)
               , Priv(..), NoPrivs(..)
               , LIO
               , getLabel, setLabelP
               , getClearance, lowerClr, lowerClrP, withClearance
               , taint, taintP
               , wguard, wguardP, aguard, aguardP
               , Labeled
               , label, labelP
               , unlabel, unlabelP
               , toLabeled, toLabeledP, discard
               , labelOf
               , taintLabeled
               , LabelFault(..)
               , catchP, onExceptionP, bracketP, handleP
               , evaluate
               , evalLIO
                ) where

import LIO.TCB hiding ( 
               LIOstate(..)
               , runLIO
               --
               , ShowTCB(..)
               , ReadTCB(..)
               , labelTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlabelTCB, setLabelTCB, lowerClrTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               , newstate, LIOstate, runLIO
               )
