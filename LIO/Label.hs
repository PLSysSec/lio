{-# LANGUAGE Safe #-}
{- | 

This module exports the type class that every label format must
implement. Labels must form a bounded lattice and be instances of
'Show' and 'Typeable'. The class 'LabelOf' is additionally used to
get the type of labeled objects.

-}

module LIO.Label ( Label(..), upperBound, lowerBound
                 , LabelOf(..) ) where

import Data.Typeable

-- | This class defines a label format, corresponding to a bounded
-- lattice. Specifically, it is necessary to define a bottom element
-- 'bottom' (in literature, written as &#8869;), a top element 'top' (in
-- literature, written as &#8868;), a join, or least upper bound, 'lub'
-- (in literature, written as &#8852;), a meet, or greatest lower bound,
-- 'glb' (in literature, written as &#8851;), and of course the
-- can-flow-to partial-order 'canFlowTo' (in literature, written as
-- &#8849;).
class (Eq l, Show l, Typeable l) => Label l where
  -- | Bottom
  bottom :: l
  -- | Top
  top :: l
  -- | /Least/ upper bound, or join, of two labels
  lub :: l -> l -> l
  -- | /Greatest/ lower bound, or meet, of two labels
  glb :: l -> l -> l
  -- | Can-flow-to relation
  canFlowTo :: l -> l -> Bool

-- | A more meaningful name for 'lub'. Note that since the name
-- does not imply /least/ upper bound it is not a method of 'Label'.
upperBound :: Label l => l -> l -> l
upperBound = lub

-- | A more meaningful name for 'glb'. Note that since the name
-- does not imply /greatest/ lower bound it is not a method of
-- 'Label'.
lowerBound :: Label l => l -> l -> l
lowerBound = glb

-- | Generic class used to ge tht type of labeled objects.
class LabelOf t where
  -- | Get the label of a type kinded @* -> *@
  labelOf :: Label l => t l a -> l
  
