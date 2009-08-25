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
                , throwL, catchL, catchLp
                , LabelFault(..)

                , LIORef, newLIORef, labelOfLIORef
                , readLIORef, writeLIORef, atomicModifyLIORef
                ) where

import LIO.TCB hiding ( 
                 lrefTCB
               , PrivTCB
               , showTCB
               , unlrefTCB, untaintioTCB, unlowerioTCB
               , getTCB, putTCB, runTCB, evalTCB
               , ioTCB
               , rethrowTCB
               )

import LIO.IOTCB
import Control.Exception
import Data.Typeable

--
-- Untrusted wrappers around TCB functions
--

onExceptionL         :: (Label l, Typeable s) =>
                        LIO l s a -> LIO l s b -> LIO l s a
onExceptionL io what = io `catchL` \e -> do what
                                            throwL (e :: SomeException)

