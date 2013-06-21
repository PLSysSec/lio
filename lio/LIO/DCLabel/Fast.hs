{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LIO.DCLabel.Fast where

import Data.Bits
import qualified Data.ByteString.Char8 as S8
import Data.Hashable
import Data.List
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

import LIO.Label
import LIO.Privs

type SetTag = Word64

--
-- Principals
--

data Principal = Principal !S8.ByteString {-# UNPACK #-} !SetTag deriving Ord

instance Show Principal where
  showsPrec _ (Principal n _) = shows n

instance Eq Principal where
  (Principal n1 t1) == (Principal n2 t2) = t1 == t2 && n1 == n2

principalName :: Principal -> S8.ByteString
{-# INLINE principalName #-}
principalName (Principal n _) = n

principalBS :: S8.ByteString -> Principal
principalBS bs = Principal bs bloom
  where hv = hash bs
        bloom = bit (hv .&. 0x3f)
                .|. (bit $ shiftR hv 6 .&. 0x3f)
                .|. (bit $ shiftR hv 12 .&. 0x3f)

principal :: String -> Principal
principal = principalBS . S8.pack

--
-- Disjunctive clauses (a.k.a. "disjunction categories")
--

data Disjunction = Disjunction !(Set Principal) {-# UNPACK #-} !SetTag

dToSet :: Disjunction -> Set Principal
dToSet (Disjunction ps _) = ps

dDump :: Disjunction -> String
dDump (Disjunction ps t) = "Disjunction " ++ showsPrec 11 ps (' ':show t)

instance Eq Disjunction where
  (Disjunction ps1 t1) == (Disjunction ps2 t2) = t1 == t2 && ps1 == ps2

instance Ord Disjunction where
  compare (Disjunction ps1 t1) (Disjunction ps2 t2)
    | otherwise = case compare (Set.size ps1) (Set.size ps2) of
                    EQ -> compare ps1 ps2
                    o  -> o

instance Show Disjunction where
  showsPrec _ (Disjunction ps _)
    | Set.size ps == 0 = ("False" ++)
    | Set.size ps == 1 = shows $ Set.findMin ps
    | otherwise = showParen True $
        foldr1 (\l r -> l . (" \\/ " ++) . r) $ map shows $ Set.toList ps

dFalse :: Disjunction
dFalse = Disjunction Set.empty 0

dSingleton :: Principal -> Disjunction
dSingleton p@(Principal _ t) = Disjunction (Set.singleton p) t

dUnion :: Disjunction -> Disjunction -> Disjunction
dUnion (Disjunction ps1 t1) (Disjunction ps2 t2) =
  Disjunction (Set.union ps1 ps2) (t1 .|. t2)

dFromList :: [Principal] -> Disjunction
dFromList pl = Disjunction (Set.fromList pl) tres
  where tres = foldl' (\tl (Principal _ tr) -> tl .|. tr) 0 pl

-- | Returns 'True' iff the first disjunction is a subset of the second.
dImplies :: Disjunction -> Disjunction -> Bool
dImplies (Disjunction ps1 t1) (Disjunction ps2 t2)
  | t1 .&. t2 /= t1 = False
  | otherwise       = ps1 `Set.isSubsetOf` ps2

--
-- Conjunctive Normal Form (CNF) Formulas
--

newtype CNF = CNF (Set Disjunction) deriving (Eq, Ord)

cToSet :: CNF -> Set Disjunction
{-# INLINE cToSet #-}
cToSet (CNF ds) = ds

instance Show CNF where
  showsPrec d (CNF ds)
    | Set.size ds == 0 = ("True" ++)
    | Set.size ds == 1 = shows $ Set.findMin ds
    | otherwise = showParen (d > 6) $
        foldr1 (\l r -> l . (" /\\ " ++) . r) $ map shows $ Set.toList ds

instance Monoid CNF where
  mempty = cTrue
  mappend = cUnion
  
cTrue :: CNF
cTrue = CNF $ Set.empty

cFalse :: CNF
cFalse = CNF $ Set.singleton dFalse

cSingleton :: Disjunction -> CNF
cSingleton = CNF . Set.singleton

setAny :: (a -> Bool) -> Set a -> Bool
setAny prd = Set.foldr' (\a -> (prd a ||)) False

setAll :: (a -> Bool) -> Set a -> Bool
setAll prd = Set.foldr' (\a -> (prd a &&)) True

cInsert :: Disjunction -> CNF -> CNF
cInsert dnew c@(CNF ds)
  | setAny (`dImplies` dnew) ds = c
  | otherwise = CNF $ Set.insert dnew $ Set.filter (not . (dnew `dImplies`)) ds

cUnion :: CNF -> CNF -> CNF
cUnion c (CNF ds) = Set.foldr cInsert c ds

cOr :: CNF -> CNF -> CNF
cOr (CNF ds1) (CNF ds2) =
  cFromList $ [dUnion d1 d2 | d1 <- Set.toList ds1, d2 <- Set.toList ds2]

cFromList :: [Disjunction] -> CNF
cFromList = Set.foldr cInsert cTrue . Set.fromList

cImplies1 :: CNF -> Disjunction -> Bool
cImplies1 (CNF ds) d = setAny (`dImplies` d) ds

cImplies :: CNF -> CNF -> Bool
cImplies c (CNF ds) = setAll (c `cImplies1`) ds

--
-- DCLabel
--

data DCLabel = DCLabel { dcSecrecy :: !CNF
                       , dcImplies :: !CNF
                       } deriving (Eq, Ord)

instance Show DCLabel where
  showsPrec d (DCLabel sec int) =
    showParen (d > 5) $ shows sec . (" %% " ++) . shows int

class ToCNF c where toCNF :: c -> CNF
instance ToCNF CNF where toCNF = id
instance ToCNF Disjunction where toCNF = cSingleton
instance ToCNF Principal where toCNF = toCNF . dSingleton
instance ToCNF [Char] where toCNF = toCNF . principal
instance ToCNF Bool where
  toCNF True = cTrue
  toCNF False = cFalse

(%%) :: (ToCNF a, ToCNF b) => a -> b -> DCLabel
a %% b = toCNF a `DCLabel` toCNF b
infix 5 %%

(/\) :: (ToCNF a, ToCNF b) => a -> b -> CNF
a /\ b = toCNF a `cUnion` toCNF b
infixl 6 /\

(\/) :: (ToCNF a, ToCNF b) => a -> b -> CNF
a \/ b = toCNF a `cOr` toCNF b
infixl 7 \/

instance Label DCLabel where
  lub (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cUnion s1 s2) (cOr i1 i2)
  glb (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cOr s1 s2) (cUnion i1 i2)
  canFlowTo (DCLabel s1 i1) (DCLabel s2 i2) = cImplies s2 s1 && cImplies i1 i2

dcMaxDowngrade :: CNF -> DCLabel -> DCLabel
dcMaxDowngrade p (DCLabel (CNF ds) int) = DCLabel sec (cUnion p int)
  where sec = CNF $ Set.filter (not . cImplies1 p) ds

instance PrivDesc DCLabel CNF where
  downgradePrivDesc = dcMaxDowngrade
  canFlowToPrivDesc p (DCLabel s1 i1) (DCLabel s2 i2) =
    cImplies (cUnion p s2) s1 && cImplies (cUnion p i1) i2
