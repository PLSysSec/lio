{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|

This module implements Disjunction Category Labels (DCLabels).
DCLabels is a label format for information flow control (IFC) systems.
This library exports relevant data types and operations that may be
used by dynamic IFC systems such as the "LIO" library.

A 'DCLabel' is a pair of /secrecy/ and /integrity/ 'Component's
(sometimes called category sets).  Each 'Component' (or formula) is a
conjunction (implemented in terms of 'Set's) of 'Clause's (or
category) in propositional logic (without negation) specifying a
restriction on the flow of information labeled as such. Alternatively,
a 'Component' can take on the value 'DCFalse' corresponding to
falsehood.  Each 'Clause', in turn, is a disjunction of 'Principal's,
, where a 'Principal' is a source of authority of type 'ByteString',
whose meaning is application-specific (e.g., a 'Principal' can be a
user name, a URL, etc.).

A clause imposes an information flow restriction. In the case of
secrecy, a clause restricts who can read, receive, or propagate the
information, while in the case of integrity it restricts who can
modify a piece of data. The principals composing a clause are said to
/own/ the clause or category.

For information to flow from a source labeled @L_1@ to a sink @L_2@, the
restrictions imposed by the clauses of @L_2@ must at least as restrictive as
all the restrictions imposed by the clauses of @L_1@ (hence the conjunction)
in the case of secrecy, and at least as permissive in the case of integrity.
More specifically, for information to flow from @L_1@ to @L_2@, the labels
must satisfy the \"can-flow-to\" relation: @L_1 &#8849; L_2@.  The &#8849;
label check is implemented by the 'canFlowTo' function.  For labels
@L_1=\<S_1, I_1\>@, @L_2=\<S_2, I_2\>@ the can-flow-to relation is satisfied
if the secrecy component @S_2@ /implies/ @S_1@ and @I_1@ /implies/ @I_2@
(recall that a category set is a conjunction of disjunctions of principals).
For example, @\<P_1 &#8897; P_2, True\> &#8849; \<P_1, True\>@ because data
that can be read by @P_1@ is more restricting than that readable by @P_1@
or @P_2@. Conversely, @\<True,P_1\> &#8849; \<True,P_1 &#8897; P_2\>@
because data vouched for by @P_1@ or @P_2@ is more permissive than just @P_1@
(note the same principle holds when writing to sinks with such labeling).

-}

module LIO.DCLabel.Core ( 
  -- * Principals
    Principal(..), principal
  -- * Clauses
  , Clause(..), clause
  -- * Components
  -- $component
  , Component(..)
  , dcTrue, dcFalse, dcFormula 
  , isTrue, isFalse
  -- * Labels
  , DCLabel(..), dcLabel, dcLabelNoReduce
  , dcPub, dcTop, dcBottom
  -- * Internal
  , dcReduce, dcImplies
  , dcAnd, dcOr
  ) where

import qualified Data.ByteString.Char8 as S8
import Data.List (intercalate)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import LIO.Label
import LIO.Privs

--
-- Principals
--

-- | A @Principal@ is a simple string representing a source of
-- authority. Any piece of code can create principals, regardless of how
-- untrusted it is.
newtype Principal = Principal { principalName :: S8.ByteString
                                -- ^ Get the principal name.
                              } deriving (Eq, Ord, Typeable)

instance Show Principal where
  showsPrec _ = shows . principalName

-- | Generate a principal from a 'String'.  (To create one from a
-- 'S8.ByteString', just use the 'Principal' constructor directly.)
principal :: String -> Principal
principal = Principal . S8.pack

--
-- Category - disjunction clauses
--

-- | A clause or disjunction category is a set of 'Principal's.
-- Logically the set corresponds to a disjunction of the principals.
newtype Clause = Clause { unClause :: Set Principal
                          -- ^ Get underlying principal-set.
                        } deriving (Eq, Typeable)

instance Ord Clause where
  (Clause c1) <= (Clause c2) =
    case () of
      _ | Set.size c1 == Set.size c2 -> c1 <= c2
      _ -> Set.size c1 < Set.size c2

instance Show Clause where
  show c@(Clause uc)
         | Set.size uc == 1 = show . head . Set.toList $ uc
         | otherwise =
              let ps = map show . Set.toList $! unClause c
              in parens . intercalate " \\/ " $! ps
    where parens x = "(" ++ x ++ ")"

-- | Clause constructor
clause :: Set Principal -> Clause
clause = Clause

{- $component
   A 'Component' is a conjunction of disjunctions of 'Principal's. A
   'DCLabel' is simply a pair of such 'Component's. Hence, we define
   almost all operations in terms of this construct, from which the
   'DCLabel' implementation follows almost trivially.
-}

-- | A component is a set of clauses, i.e., a formula (conjunction of
-- disjunction of 'Principal's). @DCFalse@ corresponds to logical
-- @False@, while @DCFormula Set.empty@ corresponds to logical @True@.
data Component = DCFalse
                 -- ^ Logical @False@
               | DCFormula { unDCFormula :: !(Set Clause) 
                              -- ^ Get underlying clause-set.
                           }
                 -- ^ Conjunction of disjunction categories
  deriving (Eq, Typeable)

instance Show Component where
  show c | isFalse c = show False
         | isTrue c  = show True
         | otherwise = let cs = map show . Set.toList $! unDCFormula c
                       in intercalate " /\\ " $! cs

-- | Privileges can be combined using 'mappend'
instance Monoid Component where
  mempty = dcTrue
  mappend p1 p2 = dcReduce $! p1 `dcAnd` p2


-- | Logical @True@.
dcTrue :: Component
dcTrue = DCFormula Set.empty

-- | Logical @False@.
dcFalse :: Component
dcFalse = DCFalse

-- | Arbitrary formula from a clause.
dcFormula :: Set Clause -> Component
dcFormula = DCFormula

-- | Is the component @True@.
isTrue :: Component -> Bool
isTrue = (== dcTrue)

-- | Is the component @False@.
isFalse :: Component -> Bool
isFalse = (== dcFalse)

--
-- Labels
--

-- | A @DCLabel@ is a pair of secrecy and integrity 'Component's.
data DCLabel = DCLabel { dcSecrecy   :: !Component
                         -- ^ Extract secrecy component of a label
                       , dcIntegrity :: !Component
                         -- ^ Extract integrity component of a label
                       } deriving (Eq, Typeable)

instance Show DCLabel where 
  showsPrec d l = showParen (d > 5) $
    let s = dcSecrecy l
        i = dcIntegrity l
    in showsPrec (d + 1) s . showString " %% " . showsPrec (d + 1) i

instance PrivDesc DCLabel Component where
  canFlowToPrivDesc pd l1 l2
           | pd == dcTrue = canFlowTo l1 l2
           | otherwise =
    let i1 = dcReduce $ dcIntegrity l1 `dcAnd` pd
        s2 = dcReduce $ dcSecrecy l2   `dcAnd` pd
    in l1 { dcIntegrity = i1 } `canFlowTo` l2 { dcSecrecy = s2 }

  partDowngradePrivDesc pd la lg
               | pd == mempty  = la `lub` lg
               | pd == dcFalse = lg
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

  downgradePrivDesc p l = partDowngradePrivDesc p l dcBottom


-- | @dcLabel secrecyComponent integrityComponent@ creates a label,
-- reducing each component to CNF.
dcLabel :: Component -> Component -> DCLabel
dcLabel c1 c2 = DCLabel (dcReduce c1) (dcReduce c2)

-- | Label contstructor. Note: the components should already be reduced.
dcLabelNoReduce :: Component -> Component -> DCLabel
dcLabelNoReduce = DCLabel



-- | Element in the DCLabel lattice corresponding to the most secret
-- and least trustworthy data.
-- @dcTop = \< False, True \> @.
dcTop :: DCLabel
dcTop = dcLabel dcFalse dcTrue

-- | Element in the DCLabel lattice corresponding to the least secret
-- and most trustworthy data.
-- @dcTop = \< True, False \> @.
dcBottom :: DCLabel
dcBottom = dcLabel dcTrue dcFalse

-- | Element in the DCLabel lattice corresponding to public data.
-- @dcPub = \< True, True \> @. This corresponds to data that is not
-- secret nor trustworthy.
dcPub :: DCLabel
dcPub = DCLabel { dcSecrecy = dcTrue, dcIntegrity = dcTrue }

--
-- Bounded by \< True, False \> and \< False, True \>
--
instance Bounded DCLabel where
  minBound = dcBottom
  maxBound = dcTop

--
-- Lattice operations
--

instance Label DCLabel where
  -- | Partial /can-flow-to/ relation on labels.
  canFlowTo l1 l2 = (dcSecrecy l2   `dcImplies` dcSecrecy l1) &&
                    (dcIntegrity l1 `dcImplies` dcIntegrity l2)


  -- | The least upper bound of two labels, i.e., the join.
  lub l1 l2 = DCLabel
    { dcSecrecy   = dcReduce $ dcSecrecy l1   `dcAnd` dcSecrecy l2
    , dcIntegrity = dcReduce $ dcIntegrity l1 `dcOr`  dcIntegrity l2 }

  -- | The greatest lower bound of two labels, i.e., the meet.
  glb l1 l2 = DCLabel
    { dcSecrecy   = dcReduce $ dcSecrecy l1   `dcOr`  dcSecrecy l2
    , dcIntegrity = dcReduce $ dcIntegrity l1 `dcAnd` dcIntegrity l2 }

--
-- Helpers
--

-- | Logical implication.
dcImplies :: Component -> Component -> Bool
dcImplies DCFalse _ = True
dcImplies _ DCFalse = False
dcImplies f1@(DCFormula cs1) f2@(DCFormula cs2)
   | isTrue f2 = True
   | isTrue f1 = False
   | otherwise = Set.foldl' dcImpliesDisj True cs2
  where dcImpliesDisj :: Bool -> Clause -> Bool
        dcImpliesDisj False _ = False
        dcImpliesDisj _ (Clause c2) = Set.foldl' f False cs1
          where f True _  = True
                f _     c1 = unClause c1 `Set.isSubsetOf` c2 

-- | Logical conjunction
dcAnd :: Component -> Component -> Component 
dcAnd x y | isFalse x || isFalse y = dcFalse
          | otherwise = DCFormula $ unDCFormula x `Set.union` unDCFormula y

-- | Logical disjunction
dcOr :: Component -> Component -> Component 
dcOr x y | isTrue x || isTrue y = dcTrue
         | isFalse x = y
         | isFalse y = x
         | otherwise = let cs1 = unDCFormula x
                           cs2 = unDCFormula y
                       in DCFormula $ doOr cs1 cs2
  where -- | Perform disjunction of two components.
        doOr :: Set Clause -> Set Clause -> Set Clause
        doOr cs1 cs2 = Set.foldl' disjFunc Set.empty cs2
          where disjFunc acc c = acc `Set.union` singleOr c cs1
        -- | Given a clause and a formula, perform logical or of
        -- clause with every clause in formula.
        singleOr :: Clause -> Set Clause -> Set Clause
        singleOr (Clause c1) = Set.map (Clause . Set.union c1 . unClause)

-- | Reduce component to conjunction normal form by removing clauses
-- implied by other.
dcReduce :: Component -> Component
dcReduce f | isFalse f || isTrue f = f
           | otherwise = DCFormula . doReduce . unDCFormula $ f
  where doReduce cs | Set.null cs = cs
        doReduce cs =
          let (x@(Clause x'), xs) = Set.deleteFindMin cs 
              ys = doReduce $ Set.filter (not . Set.isSubsetOf x' . unClause) xs
          in Set.singleton x `Set.union` ys
