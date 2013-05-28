{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies #-}

{- | 

This module exports the class 'PrivTCB' which all privilege types must
be an instance of. This class is in the TCB since privileges can be
used to bypass label restrictions and untrusted code should not be
allowed to do so arbitrarily. See "LIO.Privs" for an additional
description of privileges and their role within "LIO".

In addition to 'PrivTCB' this module exports the class 'PrivDesc'
which provides a function from privileges to /privilege descriptions/.
A privilege description is a meaningful and safe interpretation of a
coresponding privilege (note that the function must be one-to-one).
Privilege descriptions are used in "LIO.Gate" as \"proof\" of
privilege ownership.  Additionally, privilege descriptions can be used
by TCB code to mint new privileges using the 'MintTCB' class.

-}

module LIO.Privs.TCB ( 
    PrivTCB
  , PrivDesc(..)
  , MintTCB(..)
  ) where

-- | Zero-method class that imposes a restriction on what code
-- (namely trusted) can make a \"privilege type\".
class PrivTCB p

-- | Class used to convert a privilege to a privilege description.
-- This is particularly useful when one piece of code wishes to prove
-- ownership of certain privileges without granting the privilege.
-- NOTE: it is (almost) always a security violation if the privilege
-- is also the privilege description.
--
-- Although this class is not part of the TCB there are some security
-- implications that should be considered when making a type an
-- instance of this class. Specifically, if the value constructor for
-- the privilege description type @d@ is exported then some
-- trusted code must be used when \"proving\" ownership of a certain
-- privilege. This is generally a good idea even if the constructor is
-- not made available, since code can (usually) cache such privilege 
-- descriptions. An alternative is to use phantom types to enforce a
-- linear-type-like behavior.
class (PrivTCB p, Show d) => PrivDesc p d | p -> d, d -> p where
  -- | Retrive privilege description from a privilege.
  privDesc :: p -> d

-- | The dual of 'PrivDesc'. This class provides @mintTCB@ which may
-- be used to convert, or /mint/, a privilege descriptions into a
-- privilege.  Of course, @mintTCB@ must be restricted to the TCB.
class (PrivDesc p d) => MintTCB p d where
  -- | Mint a new privilege values given a privilege description.
  mintTCB :: d -> p
