{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveDataTypeable,
             MultiParamTypeClasses,
             TypeSynonymInstances #-}

{-|

This module implements the trusted compoenet of DCLabel privileges,
documented in "LIO.DCLabel.Privs".
Since privilege objects may be used unsafely, this module is marked
@-XUnsafe@. Untrusted code may access privileges using the interface
provided by "LIO.DCLabel.Privs".

-}

module LIO.DCLabel.Privs.TCB (
    DCPrivDesc
  , DCPriv
  , allPrivTCB
  ) where

import           LIO.DCLabel.Core
import           LIO.Privs
import           LIO.Privs.TCB

-- | A privilege description is simply a conjunction of disjunctions.
-- Unlike (actually minted) privileges (see 'DCPriv'), privilege
-- descriptions may be created by untrusted code.
type DCPrivDesc = Component

-- | A privilege is a minted and protected privilege description
-- ('DCPrivDesc') that may only be created by trusted code or
-- delegated from an existing @DCPriv@.
type DCPriv = Priv DCPrivDesc

-- | The all privilege corresponds to logical @False@
allPrivTCB :: DCPriv
allPrivTCB = MintTCB dcFalse

