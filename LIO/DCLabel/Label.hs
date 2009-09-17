{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |

This module implements Disjunction Category labels.

A label consists of two sets of categories, an integrity set, and a
secrecy set.  @l1 ``leq`` l2@ if and only if @l1@ contains all of the
integrity categories in @l2@ and @l2@ contains all of the integrity
categories in @l1@.

The categories themselves are Sets of Principals, where a 'Principal'
is just a 'String' whose meaning is up to the application.  Privileges
('DCPrivs') are also Principals (actually sets of Principals, but you
can only 'mintTCB' one Pinripal at a time).  Owning a Principal
(having it in a 'DCPrivs' object) confers the right to remote /any/
secrecy category containing that Principal, and add /any/ integrity
category containing that Principal.  Hence the name
/disjunction categories/:  The category {P1, P2} can be downgraded by
/either/ Principal P1 or P2.

-}

module LIO.DCLabel.Label
    (
    -- * The base label type
    Principal(..), DCType, Integrity(..), Secrecy(..)
    , DCat(..), DCSet, DCLabel(..)
    -- * Functions for categories
    , DCatI, DCatS
    , dcSingleton, dcFromList, dcUnion, dcSubsumes, dcSubsumesNE
    -- * Functions for sets of categories (DCSet)
    , dcsEmpty, dcsSingleton, dcsFromList, dcsAll
    , dcsSubsetOf, dcsUnion, dcsIntersection
    , dcsSubsumes, dcsUnion', dcsIntersection'
    -- * Functions on labels
    , dclReduce
    -- * Privileges
    , DCPrivs, dcprivs, owns
    -- * Useful aliases for LIO Monad
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

class (Show t) => DCType t where dcType :: t -> String
data Secrecy = Secrecy deriving (Eq, Show)
instance DCType Secrecy where dcType _ = "S"
data Integrity = Integrity deriving (Eq, Show)
instance DCType Integrity where dcType _ = "I"

newtype (DCType t) => DCat t = DCat (Set Principal) deriving (Eq, Ord)
type DCatI = DCat Integrity
type DCatS = DCat Secrecy

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

dcUnion                     :: (DCType t) => DCat t -> DCat t -> DCat t
dcUnion (DCat c1) (DCat c2) = DCat $ Set.union c1 c2

-- | With disjunction categories, the category {A} (owned by just A)
-- is stricter than the category {A, B} (which either A or B has the
-- right to bypass).  We therefore say that category {A} subsumes
-- category {A, B}.  Generalizing, we define @dcSubsumes c1 c2@ as the
-- subset relation:
--
-- > dcSubsumes (DCat c1) (DCat c2) = Set.isSubsetOf c1 c2
dcSubsumes                     :: (DCType t) => DCat t -> DCat t -> Bool
dcSubsumes (DCat c1) (DCat c2) = Set.isSubsetOf c1 c2

-- | Check that one category subsumes another and the two are not
-- equal.
dcSubsumesNE                     :: (DCType t) => DCat t -> DCat t -> Bool
dcSubsumesNE (DCat c1) (DCat c2) = Set.isProperSubsetOf c1 c2

--
-- Set functions
--

data (DCType t) => DCSet t = DCSet (Set (DCat t))
                           | DCAll deriving (Eq, Typeable)

dcsEmpty :: (DCType t) => DCSet t
dcsEmpty = DCSet Set.empty

dcsSingleton :: (DCType t) => DCat t -> DCSet t
dcsSingleton c = DCSet $ Set.singleton c

dcsFromList :: (DCType t) => [DCat t] -> DCSet t
dcsFromList l = DCSet $ Set.fromList l

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

-- | Produces the set of all categories that are in both sets and that
-- are subsumed by categories in both sets.
dcsIntersection'         :: (DCType t) => DCSet t -> DCSet t -> DCSet t
dcsIntersection' DCAll s = s
dcsIntersection' s DCAll = s
dcsIntersection' s1 s2   = dcsUnion' (subsumed s1 s2) (subsumed s2 s1)
    where
      subsumed a b = dcsFilter (b `hasCatSubsuming`) a

-- | 'DCSet' @s1@ /subsumes/ @s2@ iff for every category @c2@ in @s2@,
-- there is a category @c1@ in @s1@ such that @c1@ subsumes @c2@.  In
-- other words, @s1@ provides at least as much protection as @s2@.
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

dclReduce                 :: DCLabel -> DCLabel
dclReduce (DCLabel s1 i1) = DCLabel (dcsReduce s1) (dcsReduce i1)

instance Label DCLabel where
    lpure = DCLabel dcsEmpty dcsEmpty
    lclear = DCLabel dcsAll dcsEmpty
    lub (DCLabel s1 i1) (DCLabel s2 i2) =
        DCLabel (dcsUnion' s1 s2) (dcsIntersection' i1 i2)
    glb (DCLabel s1 i1) (DCLabel s2 i2) =
        DCLabel (dcsIntersection' s1 s2) (dcsUnion' i1 i2)

newtype DCPrivs = DCPrivsTCB (Set Principal) deriving (Eq, Show)

instance PrivTCB DCPrivs

instance MintTCB DCPrivs Principal where
    mintTCB p = DCPrivsTCB $ Set.singleton p

instance Monoid DCPrivs where
    mempty = DCPrivsTCB Set.empty
    mappend (DCPrivsTCB s1) (DCPrivsTCB s2) = DCPrivsTCB $ Set.union s1 s2

dcprivs                :: DCPrivs -> Set Principal
dcprivs (DCPrivsTCB s) = s

owns                         :: DCType t => DCPrivs -> DCat t -> Bool
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

-- |The base type for LIO computations using these labels.
type DC = LIO DCLabel ()

-- |Runs a computation in the LIO Monad, returning both its result,
-- and the label of the result.
evalDC :: LIO DCLabel () a -> IO (a, DCLabel)
evalDC m = evalLIO m ()
