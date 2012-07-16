{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|

This module provides bindings for the @DCLabel@ module.

-}

module LIO.DCLabel ( -- * DCLabel export
  		     module DCLabel
                     -- * Useful aliases for the LIO Monad
                   , DC, evalDC, runDC
                   ) where

import           LIO.Label
import           LIO.Monad
import           LIO.Privs
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
  labelDiffP = undefined

--
-- LIO aliases
--

-- | The monad for LIO computations using 'DCLabel' as the label.
type DC = LIO DCLabel


-- | Evaluate computation in the 'DC' monad.
evalDC :: DC a -> IO a
evalDC act = evalLIO act defaultState

-- | Evaluate computation in the 'DC' monad.
runDC :: DC a -> IO (a, LIOState DCLabel)
runDC act = runLIO act defaultState
