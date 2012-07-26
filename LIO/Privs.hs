{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances #-}
{- | 

Privileges are instances of the class called 'Priv'. They represent
the ability to bypass the protection of certain labels.  Specifically,
privilege allows you to behave as if @L_1 ``canFlowTo`` L_2@ even when
that is not the case.  The process of making data labeled @L_1@ affect
data labeled @L_2@ when @not (L_1 ``canFlowTo`` L_2)@ is called
/downgrading/.

The basic method of the 'Priv' class is 'canFlowToP', which performs a
more permissive can-flow-to check by exercising particular privileges
(in literature this relation is a pre-order, commonly written as
&#8849;&#8346;).  Almost all 'LIO' operations have variants ending
@...P@ that take a privilege argument to act in a more permissive way. 

All 'Priv' types are 'Monoid's, and so privileges can be combined with
'mappend'.  The creation of 'Priv' values is specific to the
particular label type in use;  the method used is 'mintTCB', but the
arguments depend on the particular label type. 

-}

module LIO.Privs (
  -- * Privilege descriptions
    PrivDesc(..)
  -- * Privileges
  , Priv(..)
  , NoPrivs(..)
  ) where

import Data.Monoid

import LIO.Label
import LIO.Privs.TCB

-- | This class defines privileges and the more-permissive relation
-- ('canFlowToP') on labels using privileges. Additionally, it defines
-- 'labelDiffP' which is used to compute the smallest difference between
-- two labels given a set of privilege.
class (Label l, PrivTCB p, Monoid p) => Priv l p where
    -- | The \"can-flow-to given privileges\" pre-order used to compare
    -- two labels in the presence of privileges.  If @'canFlowToP' p L_1
    -- L_2@ holds, then privileges @p@ are sufficient to downgrade data
    -- from @L_1@ to @L_2@.  Note that @'canFlowTo' L_1 L_2@ implies
    -- @'canFlowToP' p L_1 L_2@ for all @p@, but for some labels and
    -- privileges, 'canFlowToP' will hold even where 'canFlowTo' does
    -- not.
    canFlowToP :: p -> l -> l -> Bool
    canFlowToP p a b = labelDiffP p a b `canFlowTo` b

    -- | Roughly speaking, @L_r = labelDiffP p L L_g@ computes how close
    -- one can come to downgrading data labeled @L@ to the goal label
    -- @L_g@, given privileges @p@.  When @p == 'NoPrivs'@, the resulting
    -- label @L_r == L ``upperBound``L_g@.  If @p@ contains /all/
    -- possible privileges, then @L_r == L_g@.
    --
    -- More specifically, @L_r@ is the greatest lower bound of the
    -- set of all labels @L_l@ satisfying:
    --
    --   1. @ L_g &#8849; L_l@, and
    --
    --   2. @ L &#8849;&#8346; L_l@.
    --
    -- Operationally, @labelDiffP@ captures the minimum change required
    -- to the current label when viewing data labeled @L_l@.  A common
    -- pattern is to use the result of 'getLabel' as @L_g@ (i.e., the
    -- goal is to use privileges @p@ to avoid changing the label at all),
    -- and then compute @L_r@ based on the label of data the code is
    -- about to observe. 
    labelDiffP :: p  -- ^ Privileges
               -> l  -- ^ Label from which data must flow
               -> l  -- ^ Goal label
               -> l  -- ^ Result

--
-- No privileges
--

-- | Generic privilege type used to denote the lack of privileges.
data NoPrivs = NoPrivs deriving (Show, Read)

instance PrivTCB NoPrivs
instance PrivDesc NoPrivs NoPrivs where privDesc = id
instance MintTCB  NoPrivs NoPrivs where mintTCB  = id
instance Monoid NoPrivs where
  mempty      = NoPrivs
  mappend _ _ = NoPrivs

-- | With lack of privileges, 'canFlowToP' is simply 'canFlowTo', and
-- 'labelDiffP' is the least 'upperBound'.
instance Label l => Priv l NoPrivs where
  canFlowToP _ l1 l2 = l1 `canFlowTo` l2
  labelDiffP _ l lg = l `upperBound` lg
