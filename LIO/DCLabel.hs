{-| This module provides bindings for the "DCLabel" module, with some 
renaming to resolve name clashes. The delegation of privilege and 
other trusted code is not exported by this module and code wishing to
use this should import "DCLabel.TCB".
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LIO.DCLabel ( -- * DCLabel export
  		     module DCLabel.Safe
                   , DCCatSet
                     -- * Renamed privileges
                   , DCPriv, DCPrivTCB
                     -- * Useful aliases for the LIO Monad
                   , DC, evalDC
                   )where

import LIO.TCB
import Data.Typeable

import DCLabel.Safe hiding ( Label
                           , Priv
                           , bottom
                           , top
                           , join
                           , meet)
import qualified DCLabel.Core as DCL


deriving instance Typeable DCL.Disj
deriving instance Typeable DCL.Conj
deriving instance Typeable DCL.Label
deriving instance Typeable DCL.DCLabel

instance POrd DCLabel where
	leq = DCL.canflowto

instance Label DCLabel where
	lbot = DCL.bottom
	ltop = DCL.top
	lub  = DCL.join
	glb  = DCL.meet

instance PrivTCB DCL.TCBPriv

instance MintTCB DCL.TCBPriv DCL.Priv where
	mintTCB = DCL.createPrivTCB

instance MintTCB DCL.TCBPriv DCL.Principal where
	mintTCB p = DCL.createPrivTCB (newPriv p)

instance Priv DCLabel DCL.TCBPriv where
  	leqp = DCL.canflowto_p
  	lostar = undefined

--
-- Renaming
--

-- | A "DCLabel" category set.
type DCCatSet = DCL.Label
-- | A "DCLabel" (untrusted) privilege.
type DCPriv = DCL.Priv
-- | A "DCLabel" privilege.
type DCPrivTCB = DCL.TCBPriv


--
-- LIO aliases
--

-- | The monad for LIO computations using 'DCLabel' as the label.
type DC = LIO DCLabel ()

-- | Runs a computation in the LIO Monad, returning both the
-- computation's result and the label of the result.
evalDC :: DC a -> IO (a, DCLabel)
evalDC m = evalLIO m ()
