module LIO.Base ( POrdering(..), POrd(..), o2po, Label(..)
                , Lref, Priv(..)
                , lref, labelOf, taint, untaint, unlref
                , LIO
                , labelOfio, clearOfio
                , taintio, guardio, untaintio
                , lowerio, unlowerio
                , openL, closeL, discardL
                )
    where

import LIO.TCB hiding ( PrivTCB
                      , unlrefTCB, untaintioTCB, unlowerioTCB
                      , getTCB, putTCB, runTCB, evalTCB
                      , ioTCB )
