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

module LIO.DCLabel (
                   -- * The base label type
                   Principal(..), DCat(..), DCLabel(..)
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

data DCat = DCat (Set Principal) deriving (Eq, Ord)

instance Show DCat where
    showsPrec _ (DCat own) = shows (Set.toList own)

instance Read DCat where
    readsPrec _ s = do
      (own, afterown) <- reads s
      return (DCat (Set.fromList own), afterown)

data DCLabel = DCLabel { elI :: Set DCat
                       , elS :: Set DCat
                       }
             | Bottom | Top deriving (Eq, Typeable)

instance Show DCLabel where
    showsPrec _ (DCLabel i s) rest = "I=" ++ (shows (Set.toList i) $
                                      " S=" ++ shows (Set.toList s) rest)
    showsPrec _ Bottom rest        = "Bottom" ++ rest
    showsPrec _ Top rest           = "Top" ++ rest

instance Read DCLabel where
    readsPrec _ s = match "Top" Top s <|>
                    match "Bottom" Bottom s <|>
                    do (_, is) <- match "I=" () s
                       (i, afteri) <- reads is
                       (_, ss) <- match " S=" () afteri
                       (s, rest) <- reads ss
                       return (DCLabel (Set.fromList i) (Set.fromList s), rest)
        where
          match [] r s                   = [(r, s)]
          match _ _ []                   = []
          match (m:ms) r (s:ss) | m /= s = []
                                | m == s = match ms r ss   

instance POrd DCLabel where
    Bottom `leq` _                        = True
    _ `leq` Top                           = True
    _ `leq` Bottom                        = False
    Top `leq` _                           = False
    (DCLabel i1 s1) `leq` (DCLabel i2 s2) = i2 `Set.isSubsetOf` i1
                                            && s1 `Set.isSubsetOf` s2

instance Label DCLabel where
    lpure = DCLabel Set.empty Set.empty
    lclear = Top

    lub Top _    = Top
    lub _ Top    = Top
    lub Bottom x = x
    lub x Bottom = x
    lub (DCLabel i1 s1) (DCLabel i2 s2) =
                 DCLabel (Set.intersection i1 i2) (Set.union s1 s2)

    glb Bottom _ = Bottom
    glb _ Bottom = Bottom
    glb Top x    = x
    glb x Top    = x
    glb (DCLabel i1 s1) (DCLabel i2 s2) =
                 DCLabel (Set.union i1 i2) (Set.intersection s1 s2)

newtype DCPrivs = DCPrivs (Set Principal) deriving (Eq, Show)

instance PrivTCB DCPrivs

instance MintTCB DCPrivs Principal where
    mintTCB p = DCPrivs $ Set.singleton p

instance Monoid DCPrivs where
    mempty = DCPrivs Set.empty
    mappend (DCPrivs s1) (DCPrivs s2) = DCPrivs $ Set.union s1 s2

overlap :: Ord a => Set a -> Set a -> Bool
{-
overlap a b = overlap' (Set.toAscList a) (Set.toAscList b)
    where
      overlap' [] []                  = True
      overlap' (_:_) []               = False
      overlap' [] (_:_)               = False
      overlap' (a:as) (b:bs) | a == b = True
      overlap' a@(a1:as) b@(b1:bs)    = 
          if a1 < b1 then overlap' as b else overlap' a bs
-}
overlap a b = not $ Set.null (Set.intersection a b)
owns (DCPrivs p) (DCat c) = overlap p c

instance Priv DCLabel DCPrivs where
    lostar p (DCLabel li ls) (DCLabel mi ms) = DCLabel ics scs
        where
          ics = Set.filter (\c -> owns p c || Set.member c li) mi
          scs = ms `Set.union` Set.filter (not . (owns p)) ls

-- |The base type for LIO computations using these labels.
type DC = LIO DCLabel ()

-- |Runs a computation in the LIO Monad, returning both its result,
-- and the label of the result.
evalDC :: LIO DCLabel () a -> IO (a, DCLabel)
evalDC m = evalLIO m ()

{-
--
-- Testing crap
--

cat1 = DCat (Set.fromList [Principal "my@address.com"
                          , Principal "your@address.com"])
cat2 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "your@address.com"])

e = DCLabel (Set.singleton cat1) (Set.fromList [cat1, cat2])
d = DCLabel (Set.fromList [cat1, cat2]) (Set.fromList [cat1, cat2])

rl :: String -> [(DCLabel, String)]
rl = reads
-}
