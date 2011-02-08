{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
  This module implements a ``nano``, very simple, embedded domain specific language to create labels from
  conjunctions of principal disjunctions.
  
  An expression is created using the ('.\/.') and ('./\.') operators.
  The disjunction operator ('.\/.') is used to create a category from 'Principal's, 'String's,
  or a disjunctive sub-expression. For example:

  @
     p1 = 'Principal' \"p1\"
     p2 = 'Principal' \"p2\"
     p3 = 'Principal' \"p3\"
     e1 = p1 '.\/.' p2
     e2 = e1 '.\/.' \"p4\"
  @

  Similarly, the conjunction operator ('./\.') is used to create category-sets from 
  'Principals', 'Strings', and conjunctive or disjunctive sub-expressions. For example:

  @
     e3 = p1 '.\/.' p2
     e4 = e1 './\.' \"p4\" './\.' p3
  @

  /Note/ that because a category consists of a disjunction of principals, and a category set
  is composed of the conjunction of categories, ('.\/.') binds more tightly than ('./\.').

  Given an expression, one may convert it to a 'DCSet'  with 'exprTODCSet'.
  Similarly given two expressions, one for secrecy and one for integrity, one may convert it 
  to a 'DCLabel' with 'exprToDCLabel. For example:

  @
     Prelude\> 'exprToDCLabel' (\"a\" '.\/.' \"b\" './\.' \"c\") e4
     S=[[\"a\",\"b\"],[\"c\"]] I=[[\"p1\",\"p2\"],[\"p3\"],[\"p4\"]]
  @
-}


module LIO.DCLabel.NanoEDSL (
    -- * Nano very simple EDSL for working with 'DCLabels'
    PExpr(..), (./\.), (.\/.)
    -- * Converting expressions to categories, category-sets and labels
    , exprToDCat, exprToDCSet, exprToDCLabel
    -- Converting categories and category-sets to expressions
    --, dcatToPExpr, dcsetToPExpr 
    ) where

import LIO.DCLabel
import Data.Set (Set)
import qualified Data.Set as Set

infixl 7 .\/.
infixl 6 ./\.

-- | Simple type used to construct expressions.
data PExpr  = BinPExpr BinOp PExpr PExpr        -- ^ Binary expression
            | PrincipalPExpr Principal          -- ^ Principal
            | AllPrincipals                     -- ^ AllPrincipals
            | NoPrincipal                       -- ^ NoPrincipal
             deriving (Eq, Show,Read)

data BinOp = And
           | Or
           deriving (Eq, Show,Read)

-- | Turn a list of principals to an expression, either conj- or disj-unctive.
pListToPExpr :: BinOp -> [Principal] -> PExpr
pListToPExpr _ [] = NoPrincipal
pListToPExpr op (x:xs) = BinPExpr op (PrincipalPExpr x) (pListToPExpr op xs)

{-
-- Add to top description:
-- Dually, a transformation from 'DCat's and 'DCSets' to expressions is possible with 
--  'dcatToPExpr' and 'dcsetToPExpr', respectively.

-- To include these, export DCSet(..) in DCLabel.Label
-- | Turn a category to a disjunctive expression.
dcatToPExpr :: (DCType t) => DCat t -> PExpr
dcatToPExpr (DCat s) = pListToPExpr Or $ Set.toList s

-- | Turn a category set to an product-of-sums expression.
dcsetToPExpr :: (DCType t) => (DCSet t) -> PExpr
dcsetToPExpr DCAll = AllPrincipals
dcsetToPExpr (DCSet cs) = let es = map dcatToPExpr $ Set.toList cs
                          in foldr1 (BinPExpr And) es
-}

-- | Given two expressions, one for secrecy and another for integrity, convert it to a 'DCLabel'.
exprToDCLabel :: PExpr -- ^ Secrecy expression
              -> PExpr -- ^ Integrity expression
              -> DCLabel
exprToDCLabel e1 e2 = DCLabel (exprToDCSet Secrecy e1) (exprToDCSet Integrity e2)

-- | Given a 'DCType' and expression turn it into a category-set.
exprToDCSet :: (DCType t)
            => t        -- ^ Type of category-set
            -> PExpr    -- ^ Expression to convert
            -> DCSet t
exprToDCSet _ AllPrincipals = dcsAll
exprToDCSet _ NoPrincipal = dcsEmpty
exprToDCSet t (PrincipalPExpr p) = dcsSingleton $ dcFromList t [p]
exprToDCSet t expr = if isAllPrincipals expr
                       then dcsAll
                       else binExprToDCSet t expr
  where isAllPrincipals AllPrincipals = True
        isAllPrincipals NoPrincipal = False
        isAllPrincipals (PrincipalPExpr _) = False
        isAllPrincipals (BinPExpr _ e1 e2) = isAllPrincipals e1 || isAllPrincipals e2

-- | Convert a binary expression that does not have 'AllPrincipals' into a 'DCSet'.
binExprToDCSet :: (DCType t) => t -> PExpr -> DCSet t
binExprToDCSet t e@(BinPExpr Or _ _) = dcsSingleton . (dcFromList t) . disjExprToPList  $ e
binExprToDCSet t e@(BinPExpr And _ _) = dcsFromList . exprToDCatList t $ e
binExprToDCSet _ _ = error "Policy is should be described as the conjuntion of disjunctions"

-- | Turn a binary expression into a list of categories.
exprToDCatList :: (DCType t) => t -> PExpr -> [DCat t]
exprToDCatList t (BinPExpr And e1 e2) = concat . map (exprToDCatList t) $ [e1, e2]
exprToDCatList t e@(BinPExpr Or e1 e2) = [ (dcFromList t ). disjExprToPList  $ e]
exprToDCatList t (PrincipalPExpr p) = [dcFromList t [p]]
exprToDCatList _ (NoPrincipal) = []

-- | Turn a disjunctive expression to a list of principals.
disjExprToPList :: PExpr -> [Principal]
disjExprToPList (BinPExpr Or e1 e2) = disjExprToPList e1 ++ disjExprToPList e2
disjExprToPList (PrincipalPExpr p) = [p]
disjExprToPList (NoPrincipal) = []
disjExprToPList _ = error "A category should be a disjunction of principals"

-- | Turn a disunctive expression to a category.
exprToDCat :: (DCType t)
           => t        -- ^ Type of category-set
           -> PExpr    -- ^ Expression to convert
           -> DCat t
exprToDCat t = (dcFromList t) . disjExprToPList 


--
-- Nano EDSL
--


-- | Class used to create a binary disjunction expression from 'Principal's, 'PExpr's or 'String's.
class DisjunctionOf a b where
  (.\/.) :: a -> b -> PExpr

instance DisjunctionOf Principal Principal where 
 p1 .\/. p2 = pListToPExpr Or [p1,p2]

instance DisjunctionOf Principal PExpr where 
 p .\/. e = BinPExpr Or (PrincipalPExpr p) e

instance DisjunctionOf PExpr Principal where 
 e .\/. p = BinPExpr Or e (PrincipalPExpr p) 

instance DisjunctionOf PExpr PExpr where 
 e1 .\/. e2 = BinPExpr Or e1 e2

-- | Instances usng strings and not principals
instance DisjunctionOf String String where 
 p1 .\/. p2 = pListToPExpr Or $ map Principal [p1,p2]

instance DisjunctionOf String Principal where 
 p1 .\/. p2 = pListToPExpr Or [Principal p1,p2]

instance DisjunctionOf Principal String where 
 p1 .\/. p2 = pListToPExpr Or [p1,Principal p2]

instance DisjunctionOf String PExpr where 
 p .\/. e = BinPExpr Or (PrincipalPExpr . Principal $ p) e

instance DisjunctionOf PExpr String where 
 e .\/. p = BinPExpr Or e (PrincipalPExpr . Principal $ p) 

-- ---------------------------------------------------------------------------               

-- | Class used to create a binary conjunction expression from 'Principal's, 'PExpr's or 'String's.
class ConjunctionOf a b where
  (./\.) :: a -> b -> PExpr

instance ConjunctionOf Principal Principal where
  p1 ./\. p2 = pListToPExpr And [p1,p2] 

instance ConjunctionOf PExpr Principal where
  e ./\. p = BinPExpr And e (PrincipalPExpr p)

instance ConjunctionOf Principal PExpr where
  p ./\. e = BinPExpr And (PrincipalPExpr p) e

instance ConjunctionOf PExpr PExpr where
  e1 ./\. e2 = BinPExpr And e1 e2

-- | Instances usng strings and not principals
instance ConjunctionOf String String where 
 p1 ./\. p2 = pListToPExpr And $ map Principal [p1,p2]

instance ConjunctionOf String Principal where 
 p1 ./\. p2 = pListToPExpr And [Principal p1,p2]

instance ConjunctionOf Principal String where 
 p1 ./\. p2 = pListToPExpr And [p1,Principal p2]

instance ConjunctionOf String PExpr where 
 p ./\. e = BinPExpr And (PrincipalPExpr . Principal $ p) e

instance ConjunctionOf PExpr String where 
 e ./\. p = BinPExpr And e (PrincipalPExpr . Principal $ p) 

{-
-- OLD: allow for construction using DCSets and DCats
instance (DCType t) => DisjunctionOf Principal (DCat t) where 
 p .\/. c = BinPExpr Or (PrincipalPExpr p) (dcatToPExpr c)

instance (DCType t) => DisjunctionOf (DCat t) Principal where 
 c .\/. p = BinPExpr Or (dcatToPExpr c) (PrincipalPExpr p) 

instance (DCType t) => DisjunctionOf (DCat t) (DCat t) where 
 c1 .\/. c2 = BinPExpr Or (dcatToPExpr c1) (dcatToPExpr c2)

instance (DCType t) => DisjunctionOf (DCat t) PExpr where 
 c .\/. e = BinPExpr Or (dcatToPExpr c) e

instance (DCType t) => DisjunctionOf PExpr (DCat t) where 
 e .\/. c = BinPExpr Or e (dcatToPExpr c)
  
instance (DCType t) => ConjunctionOf (DCat t) Principal where
  c ./\. p = BinPExpr And (dcatToPExpr c) (PrincipalPExpr p)

instance (DCType t) => ConjunctionOf Principal (DCat t) where
  p ./\. c = BinPExpr And (PrincipalPExpr p) (dcatToPExpr c)

instance (DCType t) => ConjunctionOf Principal (DCSet t) where
  p ./\. s = BinPExpr And (PrincipalPExpr p) (dcsetToPExpr s)

instance (DCType t) => ConjunctionOf (DCSet t) Principal where
  s ./\. p = BinPExpr And (dcsetToPExpr s) (PrincipalPExpr p)

instance (DCType t) => ConjunctionOf (DCat t) (DCat t) where
  c1 ./\. c2 = BinPExpr And (dcatToPExpr c1) (dcatToPExpr c2)

instance (DCType t) => ConjunctionOf (DCSet t) (DCat t) where
  s ./\. c = BinPExpr And (dcsetToPExpr s) (dcatToPExpr c)

instance (DCType t) => ConjunctionOf (DCat t) (DCSet t) where
  c ./\. s = BinPExpr And (dcatToPExpr c) (dcsetToPExpr s)
-}
