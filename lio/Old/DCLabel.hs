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

module Old.DCLabel (
  -- ** Principals
    Principal(..), principal
  -- ** Clauses
  , Clause, clause
  -- ** Components
  , Component, dcTrue, dcFalse, dcFormula
  , isTrue, isFalse
  -- ** Labels
  , DCLabel, dcSecrecy, dcIntegrity, dcLabel
  , dcPub, dcTop, dcBottom
  -- ** Privileges
  , module Old.DCLabel.Privs
  -- ** DSL
  , module Old.DCLabel.DSL
  -- * Synonyms for "LIO"
  -- $dcMonad
  , DCState, defaultState
  , DC, evalDC, runDC, tryDC
  , MonadDC
  -- ** Labeled values
  , DCLabeled
  -- ** Labeled references
  , DCRef
  -- ** Gates
  , DCGate
  ) where

import qualified Control.Exception as IO

import LIO.Core
import LIO.Labeled
import LIO.Privs
import LIO.LIORef
import Old.DCLabel.Core
import Old.DCLabel.Privs
import Old.DCLabel.DSL

--
-- LIO synonyms
--


-- | DC 'Labeled' values.
type DCLabeled = Labeled DCLabel

-- | DC Labeled 'LIORef's.
type DCRef a = LIORef DCLabel a


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
tryDC :: DC a -> IO (Either IO.SomeException a, DCState)
tryDC act = runDC act >>= tryit
  where tryit (a, s) = do
          ea <- IO.try (IO.evaluate a)
          return (ea, s)
