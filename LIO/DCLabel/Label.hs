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
    Principal(..), DCType, Integrity, Secrecy, DCat(..), DCSet, DCLabel(..)
    -- * Category Set functions
    , dcEmpty, dcSingleton, dcFromList, dcAll
    , dcSubsetOf, dcUnion, dcIntersection
    -- * Privileges
    , DCPrivs
    -- * Useful aliases for LIO Monad
    , DC, evalDC
    ) where

import LIO.TCB

import Control.Applicative
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
dCatI   :: Set Principal -> DCat Integrity
dCatI s = DCat s
dCatS :: Set Principal -> DCat Secrecy
dCatS s = DCat s

instance (DCType t) => Show (DCat t) where
    showsPrec _ (DCat c) = shows (Set.toList c)

instance (DCType t) => Read (DCat t) where
    readsPrec _ s = do
      (own, afterown) <- reads s
      return (DCat (Set.fromList own), afterown)

data (DCType t) => DCSet t = DCSet (Set (DCat t))
                           | DCAll deriving (Eq, Typeable)

dcEmpty :: (DCType t) => DCSet t
dcEmpty = DCSet Set.empty

dcSingleton :: (DCType t) => DCat t -> DCSet t
dcSingleton c = DCSet $ Set.singleton c

dcFromList :: (DCType t) => [DCat t] -> DCSet t
dcFromList l = DCSet $ Set.fromList l

dcAll :: (DCType t) => DCSet t
dcAll = DCAll


dcSubsetOf                       :: (DCType t) => DCSet t -> DCSet t -> Bool
dcSubsetOf _ DCAll               = True
dcSubsetOf DCAll _               = False
dcSubsetOf (DCSet s1) (DCSet s2) = Set.isSubsetOf s1 s2

dcUnion                       :: (DCType t) => DCSet t -> DCSet t -> DCSet t
dcUnion DCAll _               = DCAll
dcUnion _ DCAll               = DCAll
dcUnion (DCSet s1) (DCSet s2) = DCSet $ Set.union s1 s2

dcIntersection :: (DCType t) => DCSet t -> DCSet t -> DCSet t
dcIntersection DCAll s               = s
dcIntersection s DCAll               = s
dcIntersection (DCSet s1) (DCSet s2) = DCSet $ Set.intersection s1 s2

matchskip :: String -> String -> t -> String -> [(t, String)]
matchskip skip (m:ms) r (s:ss) | m == s        = matchskip skip ms r ss   
matchskip skip m r (s:ss)      | s `elem` skip = matchskip skip m r ss
matchskip _ [] r s                             = [(r, s)]
matchskip _ _ _ _                              = []

match :: String -> t -> String -> [(t, String)]
match = matchskip ""

dcSetPrefix   :: (DCType t) => DCSet t -> String
dcSetPrefix s = gettype s undefined ++ "="
    where
      gettype     :: (DCType t) => DCSet t -> t -> String
      gettype _ t = dcType t

instance (DCType t) => Show (DCSet t) where
    showsPrec _ d@DCAll rest     = dcSetPrefix d ++ "ALL" ++ rest
    showsPrec _ d@(DCSet s) rest = dcSetPrefix d ++ shows (Set.toList s) rest

instance (DCType t) => Read (DCSet t) where
    readsPrec _ s =
        let prefix = case result of ~((r,_):_) -> dcSetPrefix r
            result = do (_, s1) <- matchskip " " prefix () s
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
    readsPrec _ s = do (i, ss) <- reads s
                       (s, rest) <- reads ss
                       return (DCLabel i s, rest)

instance POrd DCLabel where
    (DCLabel i1 s1) `leq` (DCLabel i2 s2) =
        i2 `dcSubsetOf` i1 && s1 `dcSubsetOf` s2

instance Label DCLabel where
    lpure = DCLabel dcEmpty dcEmpty
    lclear = DCLabel dcEmpty DCAll
    lub (DCLabel i1 s1) (DCLabel i2 s2) =
        DCLabel (dcIntersection i1 i2) (dcUnion s1 s2)
    glb (DCLabel i1 s1) (DCLabel i2 s2) =
        DCLabel (dcUnion i1 i2) (dcIntersection s1 s2)

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
