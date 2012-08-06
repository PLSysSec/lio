{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleContexts #-}

{- |

A data type 'Labeled' protects access to pure values (hence, we refer
to values of type @'Label' a@ as /labeled values/).  The role of
labeled values is to allow users to associate heterogeneous labels (see
"LIO.Label") with values. Although LIO\'s current label protects all
values in scope with the current label, 'Labeled' values allow for
more fine grained protection. Moreover, trusted code may easily
inspect 'Labeled' values, for instance, when inserting values into a
database.

Without the appropriate privileges, one cannot produce a pure
/unlabeled/ value that depends on a secret 'Labeled' value, or
conversely produce a high-integrity 'Labeled' value based on pure
data.  This module exports functions for creating labeled values
('label'), using the values protected by 'Labeled' by unlabeling them
('unlabel'), and changing the value of a labeled value without
inspection ('relabelLabeledP', 'taintLabeled', 'untaintLabeled').  A
'Functor'-like class ('LabeledFunctor') on 'Labeled' is also defined
in this module.

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
  -- $functor
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
-- ``canFlowTo`` l && l ``canFlowTo`` ccurrent@. Otherwise an
-- exception is thrown (see 'guardAlloc').
label :: MonadLIO l m => l -> a -> m (Labeled l a)
label = labelP NoPrivs

-- | Constructs a 'Labeled' using privilege to allow the `Labeled`'s
-- label to be below the current label.  If the current label is
-- @lcurrent@ and the current clearance is @ccurrent@, then the privilege
-- @p@ and label @l@ specified must satisfy @canFlowTo p lcurrent l@ and
-- @l ``canFlowTo`` ccurrent@.  Note that privilege is not used to bypass
-- the clearance.  You must use 'setClearanceP' to raise the clearance
-- first if you wish to create an 'Labeled' at a higher label than the
-- current clearance.
labelP :: MonadLIOP l p m => p -> l -> a -> m (Labeled l a)
labelP p l a = do
  guardAllocP p l
  return $! labelTCB l a

--
-- Unlabel values
--

-- | Within the 'LIO' monad, this function takes a 'Labeled' and returns
-- the underlying value.  Thus, in the 'LIO' monad one can say:
--
-- > x <- unlabel (xv :: Labeled SomeLabelType Int)
--
-- And now it is possible to use the value of @x :: Int@, which is the
-- pure value of what was stored in @xv@.  Of course, @unlabel@ also
-- raises the current label.  If raising the label would exceed the
-- current clearance, then @unlabel@ throws 'ClearanceViolation'.
-- However, you can use 'labelOf' to check if 'unlabel' will succeed
-- without throwing an exception.
unlabel :: MonadLIO l m => Labeled l a -> m a
unlabel = unlabelP NoPrivs

-- | Extracts the value of an 'Labeled' just like 'unlabel', but takes a
-- privilege argument to minimize the amount the current label must be
-- raised.  Function will throw 'ClearanceViolation' under the same
-- circumstances as 'unlabel'.
unlabelP :: MonadLIOP l p m => p -> Labeled l a -> m a
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
--
-- Consequently @relabelP p l lv@ throws an 'InsufficientPrivs'
-- exception if
--
-- @'canFlowToP' p l ('labelOf' lv) && 'canFlowToP' p ('labelOf' lv) l@
--
-- does not hold.
relabelLabeledP :: MonadLIOP l p m => p -> l -> Labeled l a -> m (Labeled l a)
relabelLabeledP p newl lv = do
  let origl = labelOf lv
  unless (canFlowToP p newl origl &&
          canFlowToP p origl newl) $ throwLIO InsufficientPrivs
  return . labelTCB newl $! unlabelTCB lv

-- | Raises the label of a 'Labeled' to the 'upperBound' of it's current
-- label and the value supplied.  The label supplied must be bounded by
-- the current label and clearance, though the resulting label may not be
-- if the 'Labeled' is already above the current thread's clearance. If
-- the supplied label is not bounded then @taintLabeled@ will throw an
-- exception (see 'guardAlloc').
taintLabeled :: MonadLIO l m => l -> Labeled l a -> m (Labeled l a)
taintLabeled = taintLabeledP NoPrivs

-- | Same as 'taintLabeled', but uses privileges when comparing the
-- current label to the supplied label. In other words, this function
-- can be used to lower the label of the labeled value by leveraging
-- the supplied privileges.
taintLabeledP :: MonadLIOP l p m => p -> l -> Labeled l a -> m (Labeled l a)
taintLabeledP p l lv = do
  guardAllocP p l
  return . labelTCB (l `upperBound` labelOf lv) $! unlabelTCB lv

-- | Downgrades the label of a 'Labeled' as much as possible given the
-- current privilege.
untaintLabeled :: MonadLIO l m => l -> Labeled l a -> m (Labeled l a)
untaintLabeled = untaintLabeledP NoPrivs

-- | Same as 'untaintLabeled' but uses the supplied privileges when
-- downgrading the label of the labeled value.
untaintLabeledP :: MonadLIOP l p m => p -> l -> Labeled l a -> m (Labeled l a)
untaintLabeledP p target lv =
  relabelLabeledP p (partDowngradeP p (labelOf lv) target) lv


{- $functor

Making 'Labeled' an instance of 'Functor' is problematic because:

1. 'fmap' would have type @Labeled l a -> (a -> b) -> Labeled b@ and thus 
    creating /new/ labeled values above the current clearance or below
    the current label would be feasible (given one such value).
2. 'LIO' is polymorphic in the label type and thus 'fmap' would is
   susceptible to /refinement attacks/. Superficially if the label type
   contains an integrity component (see for example "LIO.DCLabel")
   then @fmap (\ -> 3) lv@ would produce a high-integrity labeled @3@
   if @lv@ is a high-integrity labeled value without any any authority
   or /endorsement/.

As a result, we provide a class 'LabeledFunctor' that export 'lFmap'
(labeled 'lFmap') that addressed the above issues. Firstly, each newly
created value is in the 'LIO' monad and secondly each label format
implementation must produce their own definition of 'lFmap' such that
the end label protects the computation result accordingly.
-}

-- | IFC-aware functor instance. Since certain label formats may contain
-- integrity information, this is provided as a class rather than a
-- function. Such label formats will likely wish to drop endorsements in
-- the new labeled valued.
class Label l => LabeledFunctor l where
  -- | 'fmap'-like funciton that is aware of the current label and
  -- clearance.
  lFmap :: MonadLIO l m => Labeled l a -> (a -> b) -> m (Labeled l b)
