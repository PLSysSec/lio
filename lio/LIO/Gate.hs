{-# LANGUAGE Safe #-}

{- |

LIO provides a basic implementation of /gates/, useful in providing
controlled RPC-like services where the client and service provider are
in mutual distrust. 

A service provider uses 'gate' to create a gate data type @'Gate' d a@
given a computation of type @d -> a@. Here, @d@ is a privilege
description (type variable for an instance of 'PrivDesc').  Gates are
invoked with 'callGate', and as such the service provider has the
guarantee that the client (the caller) owns the privileges
corresponding to the privilege description @d@.  In effect, this
allows a client to \"prove\" to the service provider that they own
certain privileges without entrusting the service with its privileges.
The gate computation can analyze this privilege description before
performing the \"actual\" computation.  The client and server solely
need to trust the implementation of 'callGate'.

-}

module LIO.Gate (
    Gate
  , gate, callGate
  -- * Example
  -- $example
  ) where

import           LIO.Privs

-- | A Gate is a lambda abstraction from a privilege description to an
-- arbitrary type @a@. Applying the gate is accomplished with 'callGate'
-- which takes a privilege argument that is converted to a description
-- before invoking the gate computation.
newtype Gate d a = Gate { unGate :: d -> a }

-- | Create a gate given a computation from a privilege description.
-- Note that because of currying type 'a' may itself be a function
-- type and thus gates can take arguments in addition to the privilege
-- descriptoin.
gate :: PrivDesc p d
     => (d -> a)  -- ^ Gate computation
     -> Gate d a
gate = Gate

-- | Given a gate and privilege, execute the gate computation.  It is
-- important to note that @callGate@ invokes the gate computation with
-- the privilege description and /NOT/ the privilege itself.
--
-- Note that, in general, code should /not/ provide privileges to
-- functions other than @callGate@ when wishing to call a gate. This
-- function is provided by LIO since it can be easily inspected by
-- both the gate creator and caller to be doing the \"right\" thing:
-- provide the privilege description corresponding to the supplied
-- privilege as \"proof\" without explicitly passing in the privilege.
-- 
callGate :: PrivDesc l p
         => Gate p a -- ^ Gate
         -> Priv p   -- ^ Privilege used as proof-of-ownership
         -> a
callGate g = unGate g . privDesc

{- $example

This example uses "LIO.DCLabel" to demonstrate the use of gates.  The
service provider provides @addGate@ which adds two integers if the
gate is called by a piece of code that owns the \"Alice\" or \"Bob\"
principals. Otherwise, it simply returns @Nothing@.

> import LIO
> import LIO.DCLabel
> 
> import LIO.Privs.TCB (mintTCB)
> 
> 
> -- | Add two numbers if the computation is invoked by Alice or Bob.
> addGate :: DCGate (Int -> Int -> Maybe Int)
> addGate = gate $ \pd a b ->
>   if pd `elem` (dcPrivDesc `map` ["Alice", "Bob"])
>     then Just $ a + b
>     else Nothing
> 
> 
> alice, bob, clark :: DCPriv
> alice = mintTCB . dcPrivDesc $ "Alice"
> bob   = mintTCB . dcPrivDesc $ "Bob"
> clark = mintTCB . dcPrivDesc $ "Clark"
> 
> main = putStrLn . show $ 
>   [ callGate addGate alice 1 2 -- Just 3
>   , callGate addGate bob   3 4 -- Just 7
>   , callGate addGate clark 5 6 -- Nothing
>   ]


-}
