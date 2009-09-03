{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This file exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.
module LIO.Base (
                 -- * Basic label functions
                 -- $labels
                 POrdering(..), POrd(..), o2po, Label(..)
                , Priv(..), NoPrivs(..)
                -- * Labeled IO Monad (LIO)

                -- | The 'LIO' monad is a wrapper around 'IO' that
                -- keeps track of the current label and clearance.  It
                -- is possible to raise one's label or lower one's
                -- clearance without privilege, but to move in the
                -- other direction requires appropriate privilege.
                , LIO
                , currentLabel, currentClearance
                , setLabelP , setClearance, setClearanceP
                -- ** LIO guards
                -- $guards
                , taint, taintP, wguard, wguardP, aguard
                -- * References to labeled pure data (LRefs)
                , Lref
                , lref, unlrefP
                , taintR, guardR, setLabelRP
                , openR, closeR, discardR
                -- * Exceptions
                , LabelFault(..)
                , MonadCatch(..), catchP, onExceptionP
                , MonadBlock(..)
                -- * Executing computations
                , evalLIO
                -- Start TCB exports
                ) where

import LIO.TCB hiding ( 
                 ShowTCB(..)
               , lrefTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlrefTCB, setLabelTCB, setClearanceTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               )
