{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses,
             ConstraintKinds,
             TypeSynonymInstances #-}

{-|

/Disjunction Category Labels/ ('DCLabel's) are a label format that
encode secrecy and integrity using propositional logic.  This exports
label operators and instances for the "LIO". The label format is
documented in "LIO.DCLabel.Core", privileges are described in
"LIO.DCLabel.Privs", and a domain specific language for constructing
labels is presented in "LIO.DCLabel.DSL".

-}

module LIO.DCLabel (
  -- ** Principals
    Principal, principalName, principal
  -- ** Clauses
  , Clause, clause
  -- ** Components
  , Component, dcTrue, dcFalse, dcFormula
  , isTrue, isFalse
  -- ** Labels
  , DCLabel, dcSecrecy, dcIntegrity, dcLabel, dcPub
  -- ** Privileges
  , module LIO.DCLabel.Privs
  -- ** DSL
  , module LIO.DCLabel.DSL
  -- * Synonyms for "LIO"
  -- $dcMonad
  , DCState, defaultState
  , DC, evalDC, runDC, tryDC, paranoidDC
  , MonadDC
  -- ** Exceptions
  , DCLabeledException
  -- ** Labeled values
  , DCLabeled
  -- ** Labeled references
  , DCRef
  -- ** Gates
  , DCGate
  ) where

import           Control.Exception

import           LIO
import           LIO.LIORef

import           LIO.DCLabel.Core
import           LIO.DCLabel.Privs
import           LIO.DCLabel.DSL
import           LIO.DCLabel.Serialize ()

--
-- LIO synonyms
--


-- | DC Labeled exceptions.
type DCLabeledException = LabeledException DCLabel

-- | DC 'Labeled' values.
type DCLabeled = Labeled DCLabel

-- | DC Labeled 'LIORef's.
type DCRef = LIORef DCLabel


-- | DC 'Gate'.
type DCGate = Gate DCPrivDesc

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

-- | Type synonym for 'MonadLIO'.
type MonadDC m = MonadLIO DCLabel m

-- | 'LIOState' with underlying label being 'DCLabel'
type DCState = LIOState DCLabel

-- | Default, starting state for a 'DC' computation. The current label
-- is public (i.e., 'dcPub') and the current clearance is 'top'.
defaultState :: DCState
defaultState = LIOState { lioLabel = dcPub
                        , lioClearance = dcTop }

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
