{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances #-}
{- |

Privileges allow a piece of code to bypass certain information flow
restrictions imposed by labels.  A privilege is simply a conjunction
of disjunctions of 'Principal's, i.e., a 'Component'. We say that a
piece of code containing a singleton 'Clause' owns the 'Principal'
composing the 'Clause'.  However, we allow for the more general notion
of ownership of a clause, or category, as to create a
privilege-hierarchy. Specifically, a piece of code exercising a
privilege @P@ can always exercise privilege @P'@ (instead), if @P' => P@.
(This is similar to the DLM notion of \"can act for\".) Hence, if a
piece of code with certain privileges implies a clause, then it is
said to own the clause. Consequently it can bypass the restrictions of
the clause in any label.

Note that the privileges form a partial order over logicla implication
(@=>@), such that @'allPrivTCB' => P@ and @P => 'noPriv'@ for any
privilege @P@.  Hence, a privilege hierarchy which can be concretely
built through delegation, with 'allPrivTCB' corresponding to the
/root/, or all, privileges from which all others may be created. More
specifically, given a privilege @P'@ of type 'DCPriv', and a privilege
description @P@ of type 'DCPrivDesc', any piece of code can use
'delegatePriv' to \"mint\" @P@, assuming @P' => P@.

-}

module LIO.DCLabel.Privs (
    DCPrivDesc
  , DCPriv
  -- ** Helpers
  , noPriv
  , dcDelegatePriv
  , dcOwns
  ) where

import           Data.Monoid
import qualified Data.Set as Set

import           LIO
import           LIO.Privs.TCB
import           LIO.DCLabel.Core
import           LIO.DCLabel.Privs.TCB

-- | Privileges can be combined using 'mappend'
instance Monoid DCPrivDesc where
  mempty = dcTrue
  mappend p1 p2 = dcReduce $! p1 `dcAnd` p2

instance PrivDesc DCLabel DCPrivDesc where
  canFlowToPrivDesc pd l1 l2
           | pd == dcTrue = canFlowTo l1 l2
           | otherwise =
    let i1 = dcReduce $ dcIntegrity l1 `dcAnd` pd
        s2 = dcReduce $ dcSecrecy l2   `dcAnd` pd
    in l1 { dcIntegrity = i1 } `canFlowTo` l2 { dcSecrecy = s2 }

  partDowngradePrivDesc pd la lg
               | pd == mempty              = la `lub` lg
               | pd == privDesc allPrivTCB = lg
               | otherwise = 
    let sec_a  = dcSecrecy la
        int_a  = dcIntegrity la
        sec_g  = dcSecrecy   lg
        int_g  = dcIntegrity lg
        sec_a' = dcFormula . Set.filter f $ unDCFormula sec_a
        sec_res  = if isFalse sec_a
                then sec_a
                else sec_a' `dcAnd` sec_g
        int_res  = (pd `dcAnd` int_a) `dcOr` int_g
    in dcLabel sec_res int_res
      where f c = not $ pd `dcImplies` (dcFormula . Set.singleton $ c)

--
-- Helpers
--

-- | The empty privilege, or no privileges, corresponds to logical
-- @True@.
noPriv :: DCPriv
noPriv = MintTCB dcTrue

-- | Given a privilege and a privilege description turn the privilege
-- description into a privilege (i.e., mint). Such delegation succeeds
-- only if the supplied privilege implies the privilege description.
dcDelegatePriv :: DCPriv -> DCPrivDesc -> Maybe DCPriv
dcDelegatePriv p pd = let c  = privDesc $! p
                      in if c `dcImplies` pd
                           then Just $! MintTCB pd
                           else Nothing

-- | We say a piece of code having a privilege object (of type 'DCPriv')
-- owns a clause when the privileges allow code to bypass restrictions
-- imposed by the clause. This is the case if and only if the 'DCPriv'
-- object contains one of the 'Principal's in the 'Clause'.  This
-- function can be used to make such checks.
dcOwns :: DCPrivDesc -> Clause -> Bool
dcOwns pd c = pd `dcImplies` dcFormula (Set.singleton c)

