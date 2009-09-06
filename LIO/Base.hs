{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This file exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.
module LIO.Base (
                 POrdering(..), POrd(..), o2po, Label(..)
                , Priv(..), NoPrivs(..)
                , LIO
                , currentLabel, currentClearance
                , setLabelP , setClearance, setClearanceP
                , taint, taintP, wguard, wguardP, aguard
                , Lref
                , lref, lrefP, unlrefP
                , guardR
                , openR, closeR, discardR
                , LabelFault(..)
                , MonadCatch(..), catchP, onExceptionP, bracketP
                , evalLIO
                ) where

import LIO.TCB hiding ( 
                 ShowTCB(..)
               , lrefTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlrefTCB, lrefLabelTCB, setLabelTCB, setClearanceTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               )
