{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}

--
-- Disjunction Category labels
--

module LIO.DClabel ( module LIO.DClabel
                   , module LIO.Base
                   ) where

import LIO.TCB
import LIO.Base

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

data DClabel = DClabel { elI :: Set DCat
                       , elS :: Set DCat
                       }
             | Bottom | Top deriving (Eq, Typeable)

instance Show DClabel where
    showsPrec _ (DClabel i s) rest = "I=" ++ (shows (Set.toList i) $
                                      " S=" ++ shows (Set.toList s) rest)
    showsPrec _ Bottom rest        = "Bottom" ++ rest
    showsPrec _ Top rest           = "Top" ++ rest

instance Read DClabel where
    readsPrec _ s = match "Top" Top s <|>
                    match "Bottom" Bottom s <|>
                    do (_, is) <- match "I=" () s
                       (i, afteri) <- reads is
                       (_, ss) <- match " S=" () afteri
                       (s, rest) <- reads ss
                       return (DClabel (Set.fromList i) (Set.fromList s), rest)
        where
          match [] r s                   = [(r, s)]
          match _ _ []                   = []
          match (m:ms) r (s:ss) | m /= s = []
                                | m == s = match ms r ss   

instance POrd DClabel where
    Bottom `leq` _                        = True
    _ `leq` Top                           = True
    _ `leq` Bottom                        = False
    Top `leq` _                           = False
    (DClabel i1 s1) `leq` (DClabel i2 s2) = i2 `Set.isSubsetOf` i1
                                            && s1 `Set.isSubsetOf` s2

instance Label DClabel where
    lpure = DClabel Set.empty Set.empty
    lclear = Top

    lub Top _    = Top
    lub _ Top    = Top
    lub Bottom x = x
    lub x Bottom = x
    lub (DClabel i1 s1) (DClabel i2 s2) =
                 DClabel (Set.intersection i1 i2) (Set.union s1 s2)

    glb Bottom _ = Bottom
    glb _ Bottom = Bottom
    glb Top x    = x
    glb x Top    = x
    glb (DClabel i1 s1) (DClabel i2 s2) =
                 DClabel (Set.union i1 i2) (Set.intersection s1 s2)

newtype DCPrivs = DCPrivs (Set Principal) deriving (Eq, Read, Show)

instance PrivTCB DCPrivs

instance Monoid DCPrivs where
    mempty = DCPrivs Set.empty
    mappend (DCPrivs s1) (DCPrivs s2) = DCPrivs $ Set.union s1 s2


--
-- Testing crap
--

cat1 = DCat (Set.fromList [Principal "my@address.com"
                          , Principal "your@address.com"])
cat2 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "your@address.com"])

e = DClabel (Set.singleton cat1) (Set.fromList [cat1, cat2])
d = DClabel (Set.fromList [cat1, cat2]) (Set.fromList [cat1, cat2])

rl :: String -> [(DClabel, String)]
rl = reads
