{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances #-}
{- | 

Privileges are instances of the class called 'Priv'. They represent
the ability to bypass the protection of certain labels.  Specifically,
privilege allows you to behave as if @L_1 ``canFlowTo`` L_2@ even when
that is not the case.  The process of making data labeled @L_1@ affect
data labeled @L_2@ when @not (L_1 ``canFlowTo`` L_2)@ is called
/downgrading/.

The basic method of the 'Priv' class is 'canFlowToP', which performs a
more permissive can-flow-to check by exercising particular privileges
(in literature this relation is a pre-order, commonly written as
&#8849;&#8346;).  Almost all 'LIO' operations have variants ending
@...P@ that take a privilege argument to act in a more permissive way. 

All 'Priv' types are 'Monoid's, and so privileges can be combined with
'mappend'.  The creation of 'Priv' values is specific to the
particular label type in use;  the method used is 'mintTCB', but the
arguments depend on the particular label type. 

-}

module LIO.Privs (
  -- * Privilege descriptions
    PrivDesc(..), canFlowToP, partDowngradeP
  -- * Privileges
  , Priv, privDesc
  , NoPrivs, noPrivs
  ) where

import Data.Monoid
import LIO.Label
import LIO.Privs.TCB

--
-- No privileges
--

-- | Generic privilege type used to denote the lack of privileges.
data NoPrivs = NoPrivs deriving (Show, Read)

noPrivs :: Priv NoPrivs
noPrivs = MintTCB NoPrivs

instance Monoid NoPrivs where
  mempty      = NoPrivs
  mappend _ _ = NoPrivs

-- | With lack of privileges, 'canFlowToP' is simply 'canFlowTo', and
-- 'partDowngradeP' is the least 'upperBound'.
instance Label l => PrivDesc l NoPrivs where
  canFlowToPrivDesc _ l1 l2    = l1 `canFlowTo` l2
  partDowngradePrivDesc _ l lg = l `lub` lg

