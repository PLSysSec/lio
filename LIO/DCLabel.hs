{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|

/Disjunction Category Labels/ ('DCLabel's) are a label format that
encode secrecy and integrity using propositional logic.  This module
provides bindings for the "DCLabel" module and types that make it
easier and cleaner to write "LIO"+"DCLabel" code.

-}

module LIO.DCLabel (
  -- * Aliases for the LIO 
  module DCLabel
  -- ** DC monad
  -- $dcMonad
  , DC, evalDC, runDC, tryDC
  -- ** Exceptions
  , DCLabeledException
  ) where

import           LIO
import           LIO.Privs.TCB

import           DCLabel hiding (canFlowTo)
import qualified DCLabel as D
import           DCLabel.Privs.TCB



--
-- Label related instances
--

instance Label DCLabel where
  bottom    = dcBot
  top       = dcTop
  join      = dcJoin
  meet      = dcMeet
  canFlowTo = D.canFlowTo


--
-- Privileges related instances
--

instance PrivTCB  DCPriv
instance PrivDesc DCPriv DCPrivDesc where privDesc = unDCPriv
instance MintTCB  DCPriv DCPrivDesc where mintTCB = DCPrivTCB

instance Priv DCLabel DCPriv where
  canFlowToP = D.canFlowToP
  labelDiffP = error "TODO: implement labelDiffP"

--
-- LIO aliases
--


-- | DC Labeled exceptions.
type DCLabeledException = LabeledException DCLabel

--
-- DC monad
--

{- $dcMonad

The 'DC' monad is 'LIO' with using 'DCLabel's as the label format.
Most application should be written in terms of this monad, while most
libraries should remain polymorphic in the label type. It is important
that any *real* application set the initial current label and
clearance to values other than 'bottom' and 'top' as set by
'defaultState', respectively. In most cases the initial current label
should be public, i.e., 'dcPub'.
-}

-- | The monad for LIO computations using 'DCLabel' as the label.
type DC = LIO DCLabel

-- | Evaluate computation in the 'DC' monad.
evalDC :: DC a -> IO a
evalDC act = evalLIO act defaultState

-- | Evaluate computation in the 'DC' monad.
runDC :: DC a -> IO (a, LIOState DCLabel)
runDC act = runLIO act defaultState

-- | Similar to 'evalLIO', but catches any exceptions thrown by
-- untrusted code instead of propagating them.
tryDC :: DC a -> IO (Either DCLabeledException a, LIOState DCLabel)
tryDC act = tryLIO act defaultState
