{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  , DCLabel(..), dcLabel, dcLabelNoReduce, dcPub
  -- * Internal
  , dcReduce, dcImplies
  , dcAnd, dcOr
  ) where

import qualified Data.ByteString.Char8 as S8
import           Data.Typeable
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.List (intercalate)

import           LIO.Label

type S8 = S8.ByteString


--
-- Principals
--

-- | A @Principal@ is a simple string representing a source of
-- authority. Any piece of code can create principals, regardless of how
-- untrusted it is.
newtype Principal = Principal { principalName :: S8 
                                -- ^ Get the principal name.
                              } deriving (Eq, Ord, Typeable)

instance Show Principal where
  show = S8.unpack . principalName

-- | Principal constructor
principal :: S8 -> Principal
principal = Principal

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
  show c = let ps = map show . Set.toList $! unClause c
           in parens . intercalate " \\/ " $! ps
    where parens x = "[" ++ x ++ "]"

-- | Clause constructor
clause :: Set Principal -> Clause
clause = Clause

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
  show c | isFalse c = "|False"
         | isTrue c  = "|True"
         | otherwise = let cs = map show . Set.toList $! unDCFormula c
                       in parens . intercalate " /\\ " $! cs
    where parens x = "{" ++ x ++ "}"

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

{- $component
   A 'Component' is a conjunction of disjunctions of 'Principal's. A
   'DCLabel' is simply a pair of such 'Component's. Hence, we define
   almost all operations in terms of this construct, from which the
   'DCLabel' implementation follows almost trivially.
-}

-- | A @DCLabel@ is a pair of secrecy and integrity 'Component's.
data DCLabel = DCLabel { dcSecrecy   :: !Component
                         -- ^ Extract secrecy component of a label
                       , dcIntegrity :: !Component
                         -- ^ Extract integrity component of a label
                       } deriving (Eq, Typeable)

instance Show DCLabel where 
  show l = let s = dcSecrecy l
               i = dcIntegrity l
           in "< " ++ show s ++ " , " ++ show i ++ " >"

-- | Label constructor. Note that each component is first reduced to
-- CNF.
dcLabel :: Component -> Component -> DCLabel
dcLabel c1 c2 = DCLabel (dcReduce c1) (dcReduce c2)

-- | Label contstructor. Note: the components should already be reduced.
dcLabelNoReduce :: Component -> Component -> DCLabel
dcLabelNoReduce = DCLabel



-- | Element in the DCLabel lattice corresponding to public data.
-- @dcPub = \< True, True \> @. This corresponds to data that is not
-- secret nor trustworthy.
dcPub :: DCLabel
dcPub = DCLabel { dcSecrecy = dcTrue, dcIntegrity = dcTrue }

--
-- Lattice operations
--

instance Label DCLabel where
  -- | Minimal element of the DCLabel lattice, /bottom/ &#8869;, such
  -- that @&#8869; &#8849; L@ for any label @L@.
  -- Bottom is defined as: @ &#8868; = \< False, True \> @
  bottom = dcLabel dcTrue dcFalse

  -- | Maximum element of the DCLabel lattice, /top/ &#8868;,
  -- such that @L &#8849; &#8868;@ for any label @L@.
  -- Top is defined as: @ &#8868; = \< False, True \> @
  top = DCLabel dcFalse dcTrue

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
          | otherwise = DCFormula $! unDCFormula x `Set.union` unDCFormula y

-- | Logical disjunction
dcOr :: Component -> Component -> Component 
dcOr x y | isTrue x || isTrue y = dcTrue
dcOr x y | isFalse x = y
         | isFalse y = x
         | otherwise = let cs1 = unDCFormula x
                           cs2 = unDCFormula y
                       in DCFormula $! doOr cs1 cs2
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
