{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This file exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.
module LIO.Base (
                  POrdering(..), POrd(..), o2po, Label(..)
                , Lref, Priv(..), NoPrivs(..)
                , labelOf, taint, untaint, unlref
                , LIO
                , lref
                , labelOfio, clearOfio
                , taintio, guardio, cleario, untaintio
                , lowerio, unlowerio
                , openL, closeL, discardL
                , throwL, catchL, catchLp, onExceptionL
                , LabelFault(..)
                , MonadBlock(..)
                , evalLIO
                ) where

import LIO.TCB hiding ( 
                 lrefTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlrefTCB, untaintioTCB, unlowerioTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               )
