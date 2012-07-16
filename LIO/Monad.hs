{-# LANGUAGE Trustworthy #-}

{- | 

This module exports the safe interface to "LIO.Monad.TCB".
Specifically, it exports the type for the 'LIO' monad and safe
state-accessing functions.

-}

module LIO.Monad (
  -- * LIO state
    LIOState(..), defaultState
  -- * LIO Monad
  , LIO, evalLIO, runLIO
  , getLabel, getClearance
  ) where

import           LIO.Monad.TCB
import           LIO.Label
import           Control.Monad.State.Strict

--
-- LIO Monad
--


-- | Given an 'LIO' computation and some initial state, return an
-- IO action which when executed will perform the IFC-safe LIO
-- computation.
--
-- Because untrusted code cannot execute 'IO' computations, this function
-- should only be useful within trusted code.  No harm is done from
-- exposing the @evalLIO@ symbol to untrusted code.  (In general,
-- untrusted code is free to produce 'IO' computations, but it cannot
-- execute them.)
evalLIO :: Label l
        => LIO l a       
        -- ^ LIO computation
        -> LIOState l
        -- ^ Initial state
        -> IO a
evalLIO act s = evalStateT (unLIOTCB act) s

-- | Execute an 'LIO' action, returning the final state.
-- See 'evalLIO'.
runLIO :: Label l
       => LIO l a
        -- ^ LIO computation that may throw an exception
       -> LIOState l
        -- ^ Initial state
       -> IO (a, LIOState l)
runLIO act s = runStateT (unLIOTCB act) s

-- | Default 'LIO' state with the current label set to 'bottom' and
-- clearance set to 'top'.
defaultState :: Label l => LIOState l
defaultState = LIOState { lioLabel = bottom
                        , lioClearance = top 
                        , lioCallTrace = [] }

-- | Returns the current value of the thread's label.
getLabel :: Label l => LIO l l
getLabel = lioLabel `liftM` getLIOStateTCB

-- | Returns the current value of the thread's clearance.
getClearance :: Label l => LIO l l
getClearance = lioClearance `liftM` getLIOStateTCB
