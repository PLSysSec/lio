{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This file exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.
module LIO.Base (
               POrdering(..), POrd(..), o2po, Label(..)
               , Priv(..), NoPrivs(..)
               , LIO
               , currentLabel, setLabelP
               , currentClearance, setClearance, setClearanceP, withClearance
               , taint, taintP, taintL, taintLP
               , wguard, wguardP, aguard
               , Lref
               , lref, lrefP, unlrefP, labelOfR, labelOfRP
               , taintR, guardR, guardRP
               , openR, openRP, closeR, discardR
               , LrefT(..)
               , LrefD
               , lrefD, lrefPD, unlrefPD, labelOfRD
               , taintRD, openRD, openRPD
               , LabelFault(..)
               , catchP, onExceptionP, bracketP
               , evalLIO
                ) where

import LIO.TCB hiding ( 
                 ShowTCB(..)
               , lrefTCB
               , lrefDTCB, closeRDTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlrefTCB, labelOfRTCB, setLabelTCB, setClearanceTCB
               , unliftLrefTTCB, lrefTLabelTCB  
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               )
