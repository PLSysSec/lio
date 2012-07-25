{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances #-}

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
  , DCState, defaultState
  , DC, evalDC, runDC, tryDC, paranoidDC
  -- ** Exceptions
  , DCLabeledException
  -- ** Labeled values
  , DCLabeled
  -- ** Labeled references
  , DCRef
  ) where

import           LIO
import           LIO.LIORef
import           LIO.Labeled.TCB
import           LIO.Privs.TCB

import           DCLabel hiding (canFlowTo)
import qualified DCLabel as D
import           DCLabel.Privs.TCB

import           Control.Exception



--
-- Label related instances
--

instance Label DCLabel where
  bottom    = dcBot
  top       = dcTop
  lub       = dcJoin
  glb       = dcMeet
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

-- | DC Labeled values.
type DCLabeled = Labeled DCLabel

instance LabeledFunctor DCLabel where
  lFmap lv f = let s = dcSecrecy . labelOf $ lv
               in label (dcLabel s dcTrue) $ f (unlabelTCB lv)

-- | DC Labeled 'LIORef's
type DCRef = LIORef DCLabel

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


-- | 'LIOState' with underlying label being 'DCLabel
type DCState = LIOState DCLabel

-- | Default, starting state for a 'DC' computation. The current label
-- is public (i.e., 'dcPub') and the current clearance is top (i.e.,
-- 'dcTop').
defaultState :: DCState
defaultState = LIOState { lioLabel = dcPub, lioClearance = dcTop }

-- | The monad for LIO computations using 'DCLabel' as the label.
type DC = LIO DCLabel


-- | Evaluate computation in the 'DC' monad.
evalDC :: DC a -> IO a
evalDC act = evalLIO act defaultState

-- | Evaluate computation in the 'DC' monad.
runDC :: DC a -> IO (a, DCState)
runDC act = runLIO act defaultState

-- | Similar to 'evalLIO', but catches any exceptions thrown by
-- untrusted code with 'throwLIO'.
tryDC :: DC a -> IO (Either DCLabeledException a, DCState)
tryDC act = tryLIO act defaultState

-- | Similar to 'evalLIO', but catches all exceptions.
paranoidDC :: DC a -> IO (Either SomeException (a, DCState))
paranoidDC act = paranoidLIO act defaultState
