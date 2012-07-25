{-# LANGUAGE Trustworthy #-}
{- |

A data structure 'Labeled' (labeled value) protects access to pure
values.  Without the appropriate privileges, one cannot produce a pure
value that depends on a secret 'Labeled', or conversely produce a
high-integrity 'Labeled' based on pure data.  This module exports safe
functions for creating ('label', 'labelP') labeled values, using the
values protected by 'Labeled' ('unlabel', 'unlabelP') and relabeling
labeled values ('relabelLabeledP', 'taintLabeled', etc.).

-}

module LIO.Labeled (
    Labeled
  -- * Label values
  , label, labelP
  -- * Unlabel values
  , unlabel, unlabelP
  -- * Relabel values
  , relabelLabeledP
  , taintLabeled, taintLabeledP , untaintLabeled, untaintLabeledP
  -- * Labeled functor
  , LabeledFunctor(..)
  ) where

import           LIO.Label
import           LIO.Core
import           LIO.Privs
import           LIO.Labeled.TCB
import           Control.Monad

-- | Returns label of a 'Labeled' type.
instance LabelOf Labeled where
  labelOf = labelOfLabeled

--
-- Label values
--

-- | Function to construct a 'Labeled' from a label and pure value.  If
-- the current label is @lcurrent@ and the current clearance is
-- @ccurrent@, then the label @l@ specified must satisfy @lcurrent
-- ``canFlowTo`` l && l ``canFlowTo`` ccurrent@.
label :: Label l => l -> a -> LIO l (Labeled l a)
label = labelP NoPrivs

-- | Constructs a 'Labeled' using privilege to allow the `Labeled`'s
-- label to be below the current label.  If the current label is
-- @lcurrent@ and the current clearance is @ccurrent@, then the privilege
-- @p@ and label @l@ specified must satisfy @canFlowTo p lcurrent l@ and
-- @l ``canFlowTo`` ccurrent@.  Note that privilege is not used to bypass
-- the clearance.  You must use 'raiseClearanceP' to raise the clearance
-- first if you wish to create an 'Labeled' at a higher label than the
-- current clearance.
labelP :: Priv l p => p -> l -> a -> LIO l (Labeled l a)
labelP p l a = do
  guardAllocP p l
  return $! labelTCB l a

--
-- Unlabel values
--

-- | Within the 'LIO' monad, this function takes a 'Labeled' and returns
-- the value.  Thus, in the 'LIO' monad one can say:
--
-- > x <- unlabel (xv :: Labeled SomeLabelType Int)
--
-- And now it is possible to use the value of @x@, which is the pure
-- value of what was stored in @xv@.  Of course, @unlabel@ also raises
-- the current label.  If raising the label would exceed the current
-- clearance, then @unlabel@ throws 'ClearanceViolation'.
-- However, you can use 'labelOf' to check if 'unlabel' will succeed
-- without throwing an exception.
unlabel :: Label l => Labeled l a -> LIO l a
unlabel = unlabelP NoPrivs

-- | Extracts the value of an 'Labeled' just like 'unlabel', but takes a
-- privilege argument to minimize the amount the current label must be
-- raised.  Function will throw 'ClearanceViolation' under the same
-- circumstances as 'unlabel'.
unlabelP :: Priv l p => p -> Labeled l a -> LIO l a
unlabelP p lv = do
  taintP p $! labelOf lv
  return $! unlabelTCB lv

--
-- Relabel values
--

-- | Relabels a 'Labeled' value to the supplied label if the given
-- privilege privileges permits it. It must be that the original
-- label and new label are equal, modulo the supplied privileges. In
-- other words the label remains in the same congruence class.
relabelLabeledP :: Priv l p => p -> l -> Labeled l a -> LIO l (Labeled l a)
relabelLabeledP p newl lv = do
  let origl = labelOf lv
  unless (canFlowToP p newl origl &&
          canFlowToP p origl newl) $ throwLIO InsufficientPrivs
  return . labelTCB newl $! unlabelTCB lv

-- | Raises the label of a 'Labeled' to the 'upperBound' of it's current
-- label and the value supplied.  The label supplied must be bounded by the
-- current label and clearance, though the resulting label may not be if the
-- 'Labeled' is already above the current thread's clearance.
taintLabeled :: Label l => l -> Labeled l a -> LIO l (Labeled l a)
taintLabeled = taintLabeledP NoPrivs

-- | Same as 'taintLabeled', but uses privileges when comparing the
-- current label to the supplied label. In other words, this function
-- can be used to lower the label of the labeled value by leveraging
-- the supplied privileges.
taintLabeledP :: Priv l p => p -> l -> Labeled l a -> LIO l (Labeled l a)
taintLabeledP p l lv = do
  guardAllocP p l
  return . labelTCB (l `upperBound` labelOf lv) $! unlabelTCB lv

-- | Downgrades the label of a 'Labeled' as much as possible given the
-- current privilege.
untaintLabeled :: Label l => l -> Labeled l a -> LIO l (Labeled l a)
untaintLabeled = untaintLabeledP NoPrivs

-- | Same as 'untaintLabeled' but uses the supplied privileges when
-- downgrading the label of the labeled value.
untaintLabeledP :: Priv l p => p -> l -> Labeled l a -> LIO l (Labeled l a)
untaintLabeledP p target lv =
  relabelLabeledP p (labelDiffP p (labelOf lv) target) lv



-- | IFC-aware functor instance. Since certain label formats may contain
-- integrity information, this is provided as a class rather than a
-- function. Such label formats will likely wish to drop endorsements in
-- the new labeled valued.
class Label l => LabeledFunctor l where
  -- | 'fmap'-like funciton that is aware of the current label and
  -- clearance.
  lFmap :: Labeled l a -> (a -> b) -> LIO l (Labeled l b)
