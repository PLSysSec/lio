{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |

This module implements Disjunction Category labels.

A 'DCLabel' consists of two sets of categories, a secrecy set ('dclS',
of type 'DCSet' 'Secrecy') and an integrity set ('dclI', of type
'DCSet' 'Integrity').  @l1 ``leq`` l2@ if and only if @'dclS' l2@
/subsumes/ all of the secrecy restrictions implied by @'dclS' l1@ and
@'dclI' l1@ /subsumes/ all of the integrity restrictions implied by
@'dclI' l2@.  (See the 'dcsSubsumes' function for a more precise
definition of subsumes.)

The categories themselves (of type 'DCatS' and 'DCatI', for secrecy
and integrity, respectively) are Sets of 'Principal's, where a
'Principal' is just a 'String' whose meaning is up to the application.
Privileges ('DCPrivs') also correspond to 'Principal's.  You can use
'mintTCB' to obtain the privileges of a 'Principal' and 'mappend' to
combine privileges of multiple 'Principal's.  Owning a Principal
(having it in a 'DCPrivs' object) confers the right to modify labels
by removing /any/ 'DCatS' containing that Principal and adding /any/
'DCatI' containing the Principal.  Hence the name
/disjunction categories/:  The category [P1, P2] can be downgraded by
/either/ Principal P1 or P2.

-}

module LIO.DCLabel.Label
    (
    -- * The base label type
    Principal(..), DCType, Integrity(..), Secrecy(..)
    , DCat(..) , DCatI, DCatS
    , DCSet, DCLabel(..)
    -- * Functions for categories
    , dcSingleton, dcFromList, dcFromPList, dcUnion, dcSubsumes, dcSubsumesNE
    -- * Functions for sets of categories (DCSet)
    -- ** Straight set operations
    , dcsEmpty, dcsSingleton, dcsFromList, dcsFromPList, dcsAll
    , dcsSubsetOf, dcsUnion, dcsIntersection
    -- ** Operations that reflect the disjunction property
    , dcsSubsumes, dcsUnion', dcsIntersection'
    , dclReduce
    -- * Privileges
    , DCPrivs, dcprivs, owns
    -- * Useful aliases for the LIO Monad
    , DC, evalDC
    ) where

import LIO.TCB

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

newtype Principal = Principal String deriving (Eq, Ord)

instance Show Principal where
    showsPrec d (Principal s) = showsPrec d s

instance Read Principal where
    readsPrec d s = map (\(p, rest) -> (Principal p, rest)) $ readsPrec d s

-- | @DCType@ is a dumb class whose only purpose is to include both
-- the 'Secrecy' and 'Integrity' types.  This way, functions that can
-- work on both secrecy and integrity categories and sets can
-- signatures like @f :: (DCType t) => ... -> DCat t -> ... @.
class (Show t) => DCType t where dcType :: t -> String
data Secrecy = Secrecy deriving (Eq, Show)
instance DCType Secrecy where dcType _ = "S"
data Integrity = Integrity deriving (Eq, Show)
instance DCType Integrity where dcType _ = "I"

-- | @DCat@ is the generalized type for disjunction categories.  A
-- @DCat@ is a set of the 'Principals who own the category and can
-- bypass its restrictions.  The type @DCat@ must be parameterized by
-- either 'Secrecy' or 'Integrity', depending on the type of the
-- category.  ('DCatS' and 'DCatI' are handy abbreviations for these
-- parameterized types.)
newtype (DCType t) => DCat t = DCat (Set Principal) deriving (Eq, Ord)
type DCatS = DCat Secrecy
type DCatI = DCat Integrity

instance (DCType t) => Show (DCat t) where
    showsPrec _ (DCat c) = shows (Set.toList c)

instance (DCType t) => Read (DCat t) where
    readsPrec _ s = do
      (own, afterown) <- reads s
      return (DCat (Set.fromList own), afterown)

dcSingleton     :: (DCType t) => t -> Principal -> DCat t
dcSingleton _ p = DCat $ Set.singleton p

dcFromList      :: (DCType t) => t -> [Principal] -> DCat t
dcFromList _ pl = DCat $ Set.fromList pl

dcFromPList :: (DCType t) => [Principal] -> DCat t
dcFromPList pl = DCat $ Set.fromList pl

dcUnion                     :: (DCType t) => DCat t -> DCat t -> DCat t
dcUnion (DCat c1) (DCat c2) = DCat $ Set.union c1 c2

-- | With disjunction categories, the category [A] (which only A
-- 'owns') is stricter than the category [A, B] (which both A and B
-- own, and hence which either A or B can individually decide to
-- bypass).  We therefore say that category [A] subsumes category [A,
-- B].  In other words, if [A] is a secrecy category, then anyone who
-- can read data labeled [A] can also read data labeled [A, B].
-- Conversely, if [A] is an integrity category, then anyone who can
-- write data labeled [A] can also write data labeled [A, B].
--
-- Generalizing, we define @c1 `dcSubsumes` c2@ as the subset
-- relation:
-- 
-- @
--   dcSubsumes ('DCat' c1) ('DCat' c2) = c1 \`Set.isSubsetOf\` c2
-- @
dcSubsumes                     :: (DCType t) => DCat t -> DCat t -> Bool
dcSubsumes (DCat c1) (DCat c2) = Set.isSubsetOf c1 c2

-- | Returns 'True' iff the first category subsumes the second and the
-- two are not equal.
dcSubsumesNE                     :: (DCType t) => DCat t -> DCat t -> Bool
dcSubsumesNE (DCat c1) (DCat c2) = Set.isProperSubsetOf c1 c2

--
-- Set functions
--

-- | The type for representing a set of categories.  It is
-- parameterized by either 'Secrecy' or 'Integrity', depending on the
-- type of categories.  The special constructor @DCAll@ represents all
-- possible categories, and is primarily useful for the 'dclS' of the
-- default clearance--representing the fact that by default code can
-- add any secrecy categories and remove any integrity categories from
-- labels.  (See 'setClearance' and 'withClearance' for ways of being
-- less permissive.)
data (DCType t) => DCSet t = DCSet (Set (DCat t))
                           -- ^ A finite set of categories
                           | DCAll
                           -- ^ Theset of all possible categories
                             deriving (Eq, Typeable)

dcsEmpty :: (DCType t) => DCSet t
dcsEmpty = DCSet Set.empty

dcsSingleton :: (DCType t) => DCat t -> DCSet t
dcsSingleton c = DCSet $ Set.singleton c

dcsFromList :: (DCType t) => [DCat t] -> DCSet t
dcsFromList l = DCSet $ Set.fromList l

dcsFromPList :: (DCType t) => [Principal] -> DCSet t
dcsFromPList pl = dcsFromList $ map (DCat . Set.singleton $) pl

-- | The set of all possible categories of a given type (either
-- 'Secrecy' or 'Integrity').
dcsAll :: (DCType t) => DCSet t
dcsAll = DCAll


dcsSubsetOf                       :: (DCType t) => DCSet t -> DCSet t -> Bool
dcsSubsetOf _ DCAll               = True
dcsSubsetOf DCAll _               = False
dcsSubsetOf (DCSet s1) (DCSet s2) = Set.isSubsetOf s1 s2

dcsUnion                       :: (DCType t) => DCSet t -> DCSet t -> DCSet t
dcsUnion DCAll _               = DCAll
dcsUnion _ DCAll               = DCAll
dcsUnion (DCSet s1) (DCSet s2) = DCSet $ Set.union s1 s2

dcsIntersection :: (DCType t) => DCSet t -> DCSet t -> DCSet t
dcsIntersection DCAll s               = s
dcsIntersection s DCAll               = s
dcsIntersection (DCSet s1) (DCSet s2) = DCSet $ Set.intersection s1 s2

hasCatSuchThat             :: (DCType t) => DCSet t -> (DCat t -> Bool) -> Bool
hasCatSuchThat DCAll _     = True
hasCatSuchThat (DCSet s) f = isJust $ find f $ Set.toList s

forallCat     :: (DCType t) => DCSet t -> (DCat t -> Bool) -> Bool
forallCat s f = not $ s `hasCatSuchThat` (not . f)

hasCatSubsuming     :: (DCType t) => DCSet t -> DCat t -> Bool
hasCatSubsuming s c = s `hasCatSuchThat` (`dcSubsumes` c)

dcsFilter             :: (DCType t) => (DCat t -> Bool) -> DCSet t -> DCSet t
dcsFilter f (DCSet s) = DCSet $ Set.filter f s
dcsFilter _ DCAll     = DCAll

-- | Eliminate categories that are subsumed (see 'dcSubsumes') by
-- other categories in the set.
dcsReduce   :: (DCType t) => DCSet t -> DCSet t
dcsReduce s = dcsFilter needed s
    where
      needed c = not $ s `hasCatSuchThat` (`dcSubsumesNE` c)

-- | Union two sets of secrecy categories, eliminating any category in
-- a set that is subsumed by another category in that set (see
-- 'dcSubsumes').
dcsUnion'       :: (DCType t) => DCSet t -> DCSet t -> DCSet t
dcsUnion' s1 s2 = dcsReduce $ dcsUnion s1 s2

-- | @dcsIntersection' s1 s2@ produces the set of all categories @c@
-- satisfying both of the following:
--
--   1. @c@ is in at least one of @s1@ or @s2@, and
--
--   2. If one of the sets, @si@, does not contain @c@, then @si@
--      contains another category @ci@ such that
--      @ci ``dcSubsumes`` c@.
dcsIntersection'         :: (DCType t) => DCSet t -> DCSet t -> DCSet t
dcsIntersection' DCAll s = s
dcsIntersection' s DCAll = s
dcsIntersection' s1 s2   = dcsUnion' (subsumed s1 s2) (subsumed s2 s1)
    where
      subsumed a b = dcsFilter (b `hasCatSubsuming`) a

-- | 'DCSet' @s1@ /subsumes/ @s2@ iff for every category @c2@ in @s2@,
-- there is a category @c1@ in @s1@ such that @c1 ``dcSubsumes`` c2@.
--  In other words, @s1@ provides at least as much protection as
-- @s2@.
dcsSubsumes                       :: (DCType t) => DCSet t -> DCSet t -> Bool
dcsSubsumes s1 s2 =
    forallCat s2 $ \c2 -> s1 `hasCatSubsuming` c2

matchskip :: String -> String -> t -> String -> [(t, String)]
matchskip skip (m:ms) r (s:ss) | m == s        = matchskip skip ms r ss   
matchskip skip m r (s:ss)      | s `elem` skip = matchskip skip m r ss
matchskip _ [] r s                             = [(r, s)]
matchskip _ _ _ _                              = []

match :: String -> t -> String -> [(t, String)]
match = matchskip ""

matchsp :: String -> t -> String -> [(t, String)]
matchsp = matchskip " \t\r\n"

dcsPrefix   :: (DCType t) => DCSet t -> String
dcsPrefix s = gettype s undefined ++ "="
    where
      gettype     :: (DCType t) => DCSet t -> t -> String
      gettype _ t = dcType t

instance (DCType t) => Show (DCSet t) where
    showsPrec _ d@DCAll rest     = dcsPrefix d ++ "ALL" ++ rest
    showsPrec _ d@(DCSet s) rest = dcsPrefix d ++ shows (Set.toList s) rest

instance (DCType t) => Read (DCSet t) where
    readsPrec _ s =
        -- Note that prefix uses result only for its type, so as to
        -- select the right dcsPrefix function.
        let prefix = case result of ~((r,_):_) -> dcsPrefix r
            result = do (_, s1) <- matchsp prefix () s
                        (match "ALL" DCAll s1 <|>
                         do (set, rest) <- reads s1
                            return (DCSet $ Set.fromList set, rest))
        in result

data DCLabel = DCLabel { dclS :: DCSet Secrecy
                       , dclI :: DCSet Integrity
                       } deriving (Eq, Typeable)

instance Show DCLabel where
    showsPrec _ (DCLabel s i) rest = (shows s $ " " ++ shows i rest)

instance Read DCLabel where
    readsPrec _ str = do (s, str1) <- reads str
                         (i, rest) <- reads str1
                         return (DCLabel s i, rest)

instance POrd DCLabel where
    (DCLabel s1 i1) `leq` (DCLabel s2 i2) =
        s2 `dcsSubsumes` s1 && i1 `dcsSubsumes` i2
        -- i2 `dcsSubsetOf` i1 && s1 `dcsSubsetOf` s2

-- | Removes any categories that can be removed from a label without
-- changing its security implications.  Specifically, any category is
-- removed if it is subsumed by a different category that the label
-- already has.
dclReduce                 :: DCLabel -> DCLabel
dclReduce (DCLabel s1 i1) = DCLabel (dcsReduce s1) (dcsReduce i1)

instance Label DCLabel where
    lpure = DCLabel dcsEmpty dcsEmpty
    lclear = DCLabel dcsAll dcsEmpty
    lub (DCLabel s1 i1) (DCLabel s2 i2) =
        DCLabel (dcsUnion' s1 s2) (dcsIntersection' i1 i2)
    glb (DCLabel s1 i1) (DCLabel s2 i2) =
        DCLabel (dcsIntersection' s1 s2) (dcsUnion' i1 i2)

-- | 'DCPrivs' wrap a set of 'Principal's.  Having a privilege object
-- containing a 'Principal' @p@ allows one to bypass any 'DCat'
-- containing @p@.
newtype DCPrivs = DCPrivsTCB (Set Principal) deriving (Eq, Show)

instance PrivTCB DCPrivs

instance MintTCB DCPrivs Principal where
    mintTCB p = DCPrivsTCB $ Set.singleton p

instance Monoid DCPrivs where
    mempty = DCPrivsTCB Set.empty
    mappend (DCPrivsTCB s1) (DCPrivsTCB s2) = DCPrivsTCB $ Set.union s1 s2

-- | Extract the set of 'Principal's from a 'DCPrivs' object.
dcprivs                :: DCPrivs -> Set Principal
dcprivs (DCPrivsTCB s) = s

-- | We say a 'DCPrivs' privilege object /owns/ a 'DCat' when the
-- privileges allow code to bypass restrictions implied by the 'DCat'.
-- This is the case if and only if the 'DCPrivs' object contains one
-- of the 'Principal's in the 'DCat'.
owns :: DCType t =>
        DCPrivs -- ^ Privileges
     -> DCat t  -- ^ A category
     -> Bool    -- ^ Returns 'True' if Privileges can bypass category
owns (DCPrivsTCB p) (DCat c) = not $ Set.null (Set.intersection p c)

{-
dclostar :: DCPrivs -> DCLabel -> DCLabel -> DCLabel
dclostar p (DCLabel ls li) (DCLabel ms mi) =
    DCLabel ss si
    where
      hasCat DCAll _     = True
      hasCat (DCSet s) c = Set.member c s
      ss                 = dcsUnion ms $ dcsFilter needS ls
      needS c            = not (p `owns` c) && not (ms `hasCat` c)
      si                 = dcsFilter okayI mi
      okayI c            = p `owns` c || li `hasCat` c
-}

subsumesP       :: (DCType t) => DCPrivs -> DCSet t -> DCat t -> Bool
subsumesP p s c = p `owns` c || s `hasCatSubsuming` c

instance Priv DCLabel DCPrivs where
    leqp p (DCLabel s1 i1) (DCLabel s2 i2) =
        forallCat s1 (subsumesP p s2) && forallCat i2 (subsumesP p i1)

    lostar p (DCLabel ls li) (DCLabel ms mi) = DCLabel ss si
        where
          ss = dcsUnion ms $ dcsFilter (not . subsumesP p ms) ls
          si = dcsFilter (subsumesP p li) mi

-- | The monad for LIO computations using 'DCLabel' as the label.
type DC = LIO DCLabel ()

-- | Runs a computation in the LIO Monad, returning both the
-- computation's result and the label of the result.
evalDC :: DC a -> IO (a, DCLabel)
evalDC m = evalLIO m ()
