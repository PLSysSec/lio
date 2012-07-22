{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies #-}

{- | 

This module exports the class 'PrivTCB' which all privilege types
must be an instance of. This class is in the TCB since privileges can
be used to bypass label restrictions and untrusted code should not be
allowed to do this arbitrarily. See "LIO.Privs" for a description of
privileges and their role within "LIO".

In addition to 'PrivTCB' this module exports the class 'PrivDesc'
which provides a way for privileges to be represented in a meaningful
and safe way between mutually distrusting code. Such privilege
descriptions can, in turn, be used by TCB code to mint new privileges
using the 'MintTCB' class.

-}

module LIO.Privs.TCB ( 
    PrivTCB
  , PrivDesc(..)
  , MintTCB(..)
  ) where

-- | Zero-method class that imposes a restriction on what code
-- can make a type a privileges. Namely, trusted code.
class PrivTCB p

-- | Class used to describe privileges for describing privileges using
-- a type other than the privilege itself. This is particularly useful
-- when one piece of code wishes to prove ownership of certain
-- privileges without granting the privilege.
--
-- Although this class is not part of the TCB there are some security
-- implications that should be considered when making a type an
-- instance of this class. Specifically, if the value constructor for
-- the privilege description type @d@ is exported then some
-- trusted code must be used when \"proving\" ownership of a certain
-- privilege. This is generally a good idea even if the constructor is
-- not made available, since code can (usually) cache such privilege 
-- descriptions.
class (PrivTCB p, Show d) => PrivDesc p d | p -> d, d -> p where
  -- | Retrive privilege description
  privDesc :: p -> d

-- | Class used for converting privilege descriptions to privileges by
-- code that is in the TCB.
class (PrivDesc p d) => MintTCB p d where
  -- |A function that mints new privilege values
  -- in a way that only privileged code should be allowed to do
  mintTCB :: d -> p
