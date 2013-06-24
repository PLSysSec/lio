{-# LANGUAGE Unsafe #-}

{-|

This module implements the trusted compoenet of DCLabel privileges,
documented in "LIO.DCLabel.Privs".
Since privilege objects may be used unsafely, this module is marked
@-XUnsafe@. Untrusted code may access privileges using the interface
provided by "LIO.DCLabel.Privs".

-}

module Old.TCB.DCLabel (allPrivTCB) where

import Old.DCLabel.Core
import Old.DCLabel.Privs
import LIO.TCB

-- | The all privilege corresponds to logical @False@
allPrivTCB :: DCPriv
allPrivTCB = PrivTCB dcFalse

