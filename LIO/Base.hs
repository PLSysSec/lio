{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This file exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.
module LIO.Base (module LIO.TCB) where
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
