{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LIO.Privs.TCB(
  -- * Privilege descriptions
    PrivDesc(..), canFlowToP, partDowngradeP
  -- * Privileges
  , Priv(..), privDesc
  ) where

import Data.Monoid

import LIO.Label

newtype Priv a = MintTCB a deriving (Show, Eq)

instance Monoid p => Monoid (Priv p) where
  mempty = MintTCB mempty
  mappend (MintTCB m1) (MintTCB m2) = MintTCB $ m1 `mappend` m2

privDesc :: Priv a -> a
privDesc (MintTCB a) = a

-- | This class defines privileges and the more-permissive relation
-- ('canFlowToP') on labels using privileges. Additionally, it defines
-- 'partDowngradeP' which is used to downgrage a label up to a limit,
-- given a set of privilege.
class (Label l) => PrivDesc l p where
    -- | The \"can-flow-to given privileges\" pre-order used to compare
    -- two labels in the presence of privileges.  If @'canFlowToP' p L_1
    -- L_2@ holds, then privileges @p@ are sufficient to downgrade data
    -- from @L_1@ to @L_2@.  Note that @'canFlowTo' L_1 L_2@ implies
    -- @'canFlowToP' p L_1 L_2@ for all @p@, but for some labels and
    -- privileges, 'canFlowToP' will hold even where 'canFlowTo' does
    -- not.
    canFlowToPrivDesc :: p -> l -> l -> Bool
    canFlowToPrivDesc p a b = partDowngradePrivDesc p a b `canFlowTo` b

    -- | Roughly speaking, @L_r = partDowngradeP p L L_g@ computes how
    -- close one can come to downgrading data labeled @L@ to the goal
    -- label @L_g@, given privileges @p@.  When @p == 'NoPrivs'@, the
    -- resulting label @L_r == L ``lub`` L_g@.  If @p@ contains /all/
    -- possible privileges, then @L_r == L_g@.
    --
    -- More specifically, @L_r@ is the greatest lower bound of the
    -- set of all labels @L_l@ satisfying:
    --
    --   1. @ L_g &#8849; L_l@, and
    --
    --   2. @ L &#8849;&#8346; L_l@.
    --
    -- Operationally, @partDowngradeP@ captures the minimum change required
    -- to the current label when viewing data labeled @L_l@.  A common
    -- pattern is to use the result of 'getLabel' as @L_g@ (i.e., the
    -- goal is to use privileges @p@ to avoid changing the label at all),
    -- and then compute @L_r@ based on the label of data the code is
    -- about to observe. 
    partDowngradePrivDesc :: p  -- ^ Privileges
                   -> l  -- ^ Label from which data must flow
                   -> l  -- ^ Goal label
                   -> l  -- ^ Result

-- | TODO(dm): document
canFlowToP :: PrivDesc l p => Priv p -> l -> l -> Bool
canFlowToP priv = canFlowToPrivDesc (privDesc priv)

-- | TODO(dm): document
partDowngradeP :: PrivDesc l p => Priv p -> l -> l -> l
partDowngradeP priv = partDowngradePrivDesc (privDesc priv)

