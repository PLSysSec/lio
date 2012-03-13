{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- | This module exports the subset of symbols in the "LIO.TCB" module
-- that are safe for untrusted code to access.  See the "LIO.TCB"
-- module for documentation.

module LIO.Safe ( Label(..)
                 , Priv(..), noPrivs
                 , getPrivileges, withPrivileges
                 , withCombinedPrivs 
                 , dropPrivileges 
                 , LIO, LabelState
                 , evalLIO
                 , getLabel, setLabelP
                 , getClearance, lowerClr, lowerClrP, withClearance
                 , labelOf
                 , label, labelP
                 , unlabel, unlabelP
                 , taintLabeled
                 , untaintLabeled, untaintLabeledP
                 , relabelP
                 , toLabeled, toLabeledP, discard, discardP
                 , taint, taintP
                 , wguard, wguardP, aguard, aguardP
                 , Labeled
                 , LabelFault(..)
                 , catchP, handleP, onExceptionP, bracketP
                 , evaluate
                 ) where

import LIO.TCB ( Label(..)
               , Priv(..), noPrivs
               , getPrivileges, withPrivileges
               , withCombinedPrivs 
               , dropPrivileges 
               , LIO, LabelState
               , evalLIO
               , getLabel, setLabelP
               , getClearance, lowerClr, lowerClrP, withClearance
               , labelOf
               , label, labelP
               , unlabel, unlabelP
               , taintLabeled
               , untaintLabeled, untaintLabeledP
               , relabelP
               , toLabeled, toLabeledP, discard, discardP
               , taint, taintP
               , wguard, wguardP, aguard, aguardP
               , Labeled
               , LabelFault(..)
               , catchP, handleP, onExceptionP, bracketP
               , evaluate
               )
