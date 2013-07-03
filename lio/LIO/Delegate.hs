{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides two functions useful for delegating
-- privileges.  The 'delegate' function creates a 'Priv' value less
-- powerful than an existing one.  'Gate's provide a mechanism for
-- authenticating calls to closures that embed privileges.
module LIO.Delegate (
    -- * Delegating privileges
    delegate
    -- * Gates
    -- $gateIntro
  , Gate, gate, guardGate, callGate
    -- ** Gate example
    -- $gateExample
  ) where

import safe Data.Typeable

import safe LIO.Error
import safe LIO.Label
import LIO.TCB

-- | @delegate@ allows you to create a new privilege object that is
-- less powerful than an existing privilege object.  The first
-- argument supplies actual privileges.  The second argument is a
-- 'PrivDesc' describing the desired new privileges.  The call throws
-- an exception unless the privilege object supplied 'speaksFor' the
-- privilege object requested.
--
-- Note:  If you are looking for a way to create privileges /more/
-- powerful than ones you already have, you can use the 'mappend'
-- function to combine existing privileges.
delegate :: (SpeaksFor p) => Priv p -> p -> Priv p
delegate p1 p2
  | privDesc p1 `speaksFor` p2 = PrivTCB p2
  | otherwise                  = insufficientPrivs "delegate" (privDesc p1) p2

{- $gateIntro

LIO provides a basic implementation of /gates/, useful in providing
controlled RPC-like services where the client and service provider are
in mutual distrust. 

A service provider uses 'gate' to create a gate data type @'Gate' p a@
given a computation of type @p -> a@. Here, @p@ is a privilege
description (type variable for an instance of 'PrivDesc').  Gates are
invoked with 'callGate', and as such the service provider has the
guarantee that the client (the caller) owns the privileges
corresponding to the privilege description @p@.  In effect, this
allows a client to \"prove\" to the service provider that it owns
certain privileges without actually entrusting the service with these
privileges.  The gate computation can analyze this privilege
description before performing the \"actual\" computation.  The
'speaksFor' function may be useful.  When supplied privileges are
insufficient, the gate code can raise an exception with
'insufficientPrivs'.

Note that the client and server must both trust the implementation of
'callGate', which is why it is part of the LIO library, even though
the function itself is only one line of code.

-}


-- | A Gate is a lambda abstraction from a privilege description to an
-- arbitrary type @a@. Applying the gate is accomplished with 'callGate'
-- which takes a privilege argument that is converted to a description
-- before invoking the gate computation.
newtype Gate p a = GateTCB (p -> a) deriving Typeable
-- Note GateTCB is trusted by convention.  Anyone with access to the
-- symbol can call any gate while claiming arbitrary privileges.  In
-- the absence of gates, however, GateTCB doesn't provide any
-- particular privileges.

-- | Create a gate given a computation from a privilege description.
-- Note that because of currying type 'a' may itself be a function
-- type and thus gates can take arguments in addition to the privilege
-- descriptoin.
gate :: (p -> a)  -- ^ Gate computation
     -> Gate p a
{-# INLINE gate #-}
gate = GateTCB

-- | @guardGate name minPriv a@ creates a simple gate that requires
-- privileges at least as high as @minPriv@ to return the payload or
-- function @a@.  If the privileges supplied are insufficient, an
-- exception of type 'InsufficientPrivs' is thrown.  The argument
-- @name@ is used only when an exception is thrown, to make the source
-- of the exception more easily traceable.
--
-- > guardGate name minPriv a = gate $ \pd ->
-- >   if pd `speaksFor` minPriv then a
-- >   else insufficientPrivs name pd minPriv
guardGate :: (SpeaksFor p) => String -> p -> a -> Gate p a
guardGate name minPriv a = gate $ \pd ->
  if pd `speaksFor` minPriv then a
  else insufficientPrivs name pd minPriv

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
callGate :: Gate p a -- ^ Gate
         -> Priv p   -- ^ Privilege used as proof-of-ownership
         -> a
{-# INLINE callGate #-}
callGate (GateTCB g) = g . privDesc

{- $gateExample

This example uses "LIO.DCLabel" to demonstrate the use of gates.  The
service provider provides @addGate@ which adds two integers if the
gate is called by a piece of code that owns the \"Alice\" or \"Bob\"
principals. Otherwise, it simply returns @Nothing@.  Though 'privInit'
(in "LIO.Run#v:privInit") allows us to create arbitrary privileges in
the 'IO' monad, the gate restricts access to @addGate@ from within the
'LIO' monad where code cannot create arbitrary privileges.

> import LIO
> import LIO.DCLabel
> import LIO.Run
> 
> -- | Add two numbers if the computation is invoked by Alice or Bob.
> addGate :: Gate CNF (Int -> Int -> Maybe Int)
> addGate = gate $ \pd a b ->
>   if pd `speaksFor` "Alice" \/ "Bob"
>   then Just $ a + b
>   else Nothing
> 
> main :: IO ()
> main = do
>   alice <- privInit $ toCNF $ "Alice"
>   bob <- privInit $ toCNF $ "Bob"
>   clark <- privInit $ toCNF $ "Clark"
>   putStrLn . show $ [
>       callGate addGate alice 1 2 -- Just 3
>     , callGate addGate bob   3 4 -- Just 7
>     , callGate addGate clark 5 6 -- Nothing
>     ]
> 

-}

