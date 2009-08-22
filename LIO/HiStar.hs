{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module LIO.HiStar ( module LIO.HiStar
                  , module LIO.Base
                  ) where

import LIO.TCB
import LIO.Base

import Data.Monoid
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

--
-- Some generic code
--

withDefaults               :: a -> a -> (a -> a -> b) -> Maybe a -> Maybe a
                           -> b
withDefaults d1 d2 f a1 a2 = f (unJust d1 a1) (unJust d2 a2)
    where
      unJust             :: a -> Maybe a -> a
      unJust _ (Just v)  = v
      unJust def Nothing = def

assocs2 :: Ord k => (Map k v1) -> (Map k v2) -> [(k, Maybe v1, Maybe v2)]
assocs2 m1 m2 = merge (Map.assocs m1) (Map.assocs m2)
    where
      merge [] [] = []
      merge ((kx, vx):xs) [] = (kx, Just vx, Nothing) : merge xs []
      merge [] ((ky, vy):ys) = (ky, Nothing, Just vy) : merge [] ys
      merge x@((kx, vx):xs) y@((ky, vy):ys) =
          case compare kx ky of
            EQ -> (kx, Just vx, Just vy) : merge xs ys
            LT -> (kx, Just vx, Nothing) : merge xs y
            GT -> (ky, Nothing, Just vy) : merge x ys

mergeWith         :: Ord k =>
                     (Maybe a -> Maybe b -> Maybe c) -> Map k a -> Map k b
                  -> Map k c
mergeWith f m1 m2 = domerge Map.empty $ assocs2 m1 m2
    where
      domerge m []               = m
      domerge m ((k, v1, v2):as) = domerge (action k (f v1 v2) m) as
      action k Nothing m = m
      action k (Just v) m = Map.insert k v m


--
-- Now for the HSLabel Label
--

newtype HSCategory = HSC Integer deriving (Eq, Ord, Show)

{-
instance Enum HSCategory where
    toEnum i = HSC i
    fromEnum (HSC i) = i
-}

data HSLevel = L0 | L1 | L2 | L3 deriving (Eq, Ord, Enum, Show)

instance POrd HSLevel where
    pcompare a b = o2po $ compare a b

-- Second component of HSLabel is the default level for categories not
-- in the map.  Invariant:  Map must not contain any entries mapping
-- categories to the default level.
data HSLabel = HSL (Map HSCategory HSLevel) HSLevel deriving (Show)

instance Eq HSLabel where
    a == b = pcompare a b == PEQ

instance POrd HSLabel where
    pcompare (HSL m1 d1) (HSL m2 d2) = foldl each mempty (assocs2 m1 m2)
        where
          each r (k, v1, v2) = r `mappend` comp v1 v2
          comp = withDefaults d1 d2 pcompare
          
combineLabel                            :: (HSLevel -> HSLevel -> HSLevel)
                                        -> HSLabel -> HSLabel -> HSLabel
combineLabel fn (HSL m1 d1) (HSL m2 d2) =
    HSL (mergeWith combiner m1 m2) d
    where
      d = fn d1 d2
      no_d v | v == d = Nothing
             | otherwise = Just v
      combiner v1 v2 = no_d $ withDefaults d1 d2 fn v1 v2

instance Label HSLabel where 
    lpure  = HSL Map.empty L1
    lclear = HSL Map.empty L3
    lub    = combineLabel max
    glb    = combineLabel min


--
-- Functions for manipulating labels
--

lupdate                   :: HSLabel -> HSCategory -> HSLevel -> HSLabel
lupdate (HSL m d) cat lev = HSL m' d
    where
      m' = if lev == d then Map.delete cat m else Map.insert cat lev m

lupdates              :: HSLabel -> [HSCategory] -> HSLevel -> HSLabel
lupdates lab cats lev = foldl (\lab' cat -> lupdate lab' cat lev) lab cats
                                    
lapply               :: HSLabel -> HSCategory -> HSLevel
lapply (HSL m d) cat = Map.findWithDefault d cat m

lcat L0 c = HSL (Map.singleton c L0) L3
lcat L2 c = HSL (Map.singleton c L2) L0
lcat L3 c = HSL (Map.singleton c L3) L0

newtype HSPrivs = HSPrivs [HSCategory]
data HSState = HSState { nextCat :: HSCategory }
type HS a = LIO HSLabel HSState a

instance Monoid HSPrivs where
    mempty                          = HSPrivs []
    mappend (HSPrivs a) (HSPrivs b) = HSPrivs $ union a b

instance Priv HSLabel HSPrivs where
    lostar (HSPrivs p) l = lupdates l p L0
    histar (HSPrivs p) l = lupdates l p L3

newcat     :: HSLevel -> HS (HSPrivs, HSLabel)
newcat lev = do ls <- getLS
                let cat = nextCat ls
                    HSC uncat = cat
                    lab = lcat lev cat
                    ncat = HSC $ uncat + 1
                putLS ls { nextCat = ncat }
                return (HSPrivs [cat], lab)

newHS = HSState { nextCat = HSC 100 }

evalHS :: HS t -> IO (t, HSLabel)
evalHS = evalLIO newHS

