{-# LANGUAGE Safe #-}

{- |

LIO provides a basic implementation of /gates/, useful in providing
controlled RPC-like services where the client and service provider are
in mutual distrust. 

A service provider uses 'gate' to create a gate data type 'Gate d a'.
The gate computation is of type @d -> a@, where @d@, a privilege
description (type variable for an instance of 'PrivDesc').  Since
gates are invoked with 'callGate', the service provider has the
guarantee that the client (the caller) owns the privileges
corresponding to the privilege description @d@. In effect, this allows
a client to \"prove\" to the service provider that they own certain
privileges without entrusting the service with its privileges.  The
gate computation can analyze this privilege description when
determining the result.

-}

module LIO.Gate (
    Gate
  , gate, callGate
  ) where

import           LIO.Privs

-- | A Gate is a protected computation.
newtype Gate d a = Gate { unGate :: d -> a }

-- | Create a gate given a computation from a privilege description.
gate :: PrivDesc p d
     => (d -> a)  -- ^ Gate computation
     -> Gate d a
gate = Gate

-- | Given a gate and privilege, execute the gate computation.  It is
-- important to note that @callGate@ invokes the gate computation with
-- the privilege description and /not/ the actual privilege.
--
-- Note that, in general, code should /not/ provide privileges to
-- functions other than @callGate@ when wishing to call a gate. This
-- function is provided by LIO since it can be easily inspected by
-- both the gate creator and caller to be doing the \"right\" thing:
-- provide the privilege description corresponding to the supplied
-- privilege as \"proof\" without explicitly passing in the privilege.
-- 
callGate :: PrivDesc p d
         => Gate d a -- ^ Gate
         -> p        -- ^ Privilege used as proof-of-ownership
         -> a
callGate g = unGate g . privDesc
