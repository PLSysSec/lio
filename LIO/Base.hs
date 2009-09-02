{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This file exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.
module LIO.Base (
                -- * Basic label functions
                -- $labels
                POrdering(..), POrd(..), o2po, Label(..)
                , Lref, Priv(..), NoPrivs(..)
                , taintR, setLabelRP, unlrefP
                -- * Labeled IO Monad (LIO)
                , LIO
                , lref
                , currentLabel, currentClearance
                , taint, taintP, wguard, wguardP
                , aguard, setLabelP
                , setClearance, setClearanceP
                , openL, closeL, discardL
                -- * Exceptions
                , throwL, catchL, catchLp, onExceptionL
                , LabelFault(..)
                , MonadBlock(..)
                -- * Executing computations
                , evalLIO
                -- Start TCB exports
                ) where
-- XXX - just for now

import LIO.TCB hiding ( 
                 lrefTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlrefTCB, setLabelTCB, setClearanceTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               )
