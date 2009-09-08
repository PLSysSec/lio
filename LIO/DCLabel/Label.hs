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
    Principal(..), DCType, Integrity, Secrecy
    , DCat(..), DCSet, DCLabel(..)
    -- * Functions for categories
    , DCatI, DCatS
    , dcSingleton, dcFromList, dcUnion, dcSubsumes, dcSubsumesNE
    -- * Functions for sets of categories (DCSet)
    , dcsEmpty, dcsSingleton, dcsFromList, dcsAll
    , dcsSubsetOf, dcsUnion, dcsIntersection
    , dcsSubsumes
    -- * Privileges
    , DCPrivs
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

dcsReduce              :: (DCType t) => DCSet t -> DCSet t
dcsReduce DCAll        = DCAll
dcsReduce s@(DCSet ss) =
    DCSet $ Set.filter needed ss
    where
      needed c = not $ s `hasCatSuchThat` (`dcSubsumesNE` c)
                        

-- | 'DCSet' @s1@ /subsumes/ @s2@ iff for every category @c2@ in @s2@,
-- there is a category @c1@ in @s1@ such that @c1@ subsumes @c2@.  In
-- other words, @s1@ provides at least as much protection as @s2@.
dcsSubsumes                       :: (DCType t) => DCSet t -> DCSet t -> Bool
dcsSubsumes s1 s2 =
    forallCat s2 $ \c2 -> s1 `hasCatSuchThat` (`dcSubsumes` c2)
    where
      forallCat s f = not $ s `hasCatSuchThat` (not . f)
      

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

data DCLabel = DCLabel { elI :: DCSet Integrity
                       , elS :: DCSet Secrecy
                       } deriving (Eq, Typeable)

instance Show DCLabel where
    showsPrec _ (DCLabel i s) rest = (shows i $ " " ++ shows s rest)

instance Read DCLabel where
    readsPrec _ str = do (i, ss) <- reads str
                         (s, rest) <- reads ss
                         return (DCLabel i s, rest)

instance POrd DCLabel where
    (DCLabel i1 s1) `leq` (DCLabel i2 s2) =
        i2 `dcsSubsetOf` i1 && s1 `dcsSubsetOf` s2

instance Label DCLabel where
    lpure = DCLabel dcsEmpty dcsEmpty
    lclear = DCLabel dcsEmpty DCAll
    lub (DCLabel i1 s1) (DCLabel i2 s2) =
        DCLabel (dcsIntersection i1 i2) (dcsUnion s1 s2)
    glb (DCLabel i1 s1) (DCLabel i2 s2) =
        DCLabel (dcsUnion i1 i2) (dcsIntersection s1 s2)

newtype DCPrivs = DCPrivs (Set Principal) deriving (Eq, Show)

instance PrivTCB DCPrivs

instance MintTCB DCPrivs Principal where
    mintTCB p = DCPrivs $ Set.singleton p

instance Monoid DCPrivs where
    mempty = DCPrivs Set.empty
    mappend (DCPrivs s1) (DCPrivs s2) = DCPrivs $ Set.union s1 s2

owns                      :: DCType t => DCPrivs -> DCat t -> Bool
owns (DCPrivs p) (DCat c) = not $ Set.null (Set.intersection p c)

instance Priv DCLabel DCPrivs where
    lostar p (DCLabel li ls) (DCLabel mi ms) =
        DCLabel (doi li mi) (dos ls ms)
        where
          doi (DCSet li) (DCSet mi) =
              DCSet $ Set.filter (\c -> owns p c || Set.member c li) mi
          doi s DCAll = s
          doi DCAll s = s

          dos (DCSet ls) (DCSet ms) =
              DCSet $ ms `Set.union` Set.filter (not . (owns p)) ls
          dos _ _ = DCAll

-- |The base type for LIO computations using these labels.
type DC = LIO DCLabel ()

-- |Runs a computation in the LIO Monad, returning both its result,
-- and the label of the result.
evalDC :: LIO DCLabel () a -> IO (a, DCLabel)
evalDC m = evalLIO m ()
