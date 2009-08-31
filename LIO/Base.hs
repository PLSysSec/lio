module LIO.Base (
                  POrdering(..), POrd(..), o2po, Label(..)
                , Lref, Priv(..)
                , labelOf, taint, untaint, unlref
                , LIO
                , lref
                , labelOfio, clearOfio
                , taintio, guardio, cleario, untaintio
                , lowerio, unlowerio
                , openL, closeL, discardL
                , throwL, catchL, catchLp, onExceptionL
                , LabelFault(..)
                ) where

import LIO.TCB hiding ( 
                 lrefTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlrefTCB, untaintioTCB, unlowerioTCB
               , getTCB, putTCB, runTCB, evalTCB
               , ioTCB, rtioTCB
               , rethrowTCB
               )
