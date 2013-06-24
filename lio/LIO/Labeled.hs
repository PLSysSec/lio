{-# LANGUAGE Trustworthy #-}

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
inspection ('relabelLabeledP', 'taintLabeled').  A
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
  , taintLabeled, taintLabeledP 
  , lFmap, lAp
  ) where

import safe Control.Monad

import safe LIO.Error
import safe LIO.Label
import safe LIO.Core
import LIO.TCB

--
-- Label values
--

-- | Function to construct a 'Labeled' from a label and pure value.  If
-- the current label is @lcurrent@ and the current clearance is
-- @ccurrent@, then the label @l@ specified must satisfy @lcurrent
-- ``canFlowTo`` l && l ``canFlowTo`` ccurrent@. Otherwise an
-- exception is thrown (see 'guardAlloc').
label :: Label l => l -> a -> LIO l (Labeled l a)
label l a = do
  withContext "label" $ guardAlloc l
  return $ LabeledTCB l a

-- | Constructs a 'Labeled' using privilege to allow the `Labeled`'s
-- label to be below the current label.  If the current label is
-- @lcurrent@ and the current clearance is @ccurrent@, then the privilege
-- @p@ and label @l@ specified must satisfy @canFlowTo p lcurrent l@ and
-- @l ``canFlowTo`` ccurrent@.  Note that privilege is not used to bypass
-- the clearance.  You must use 'setClearanceP' to raise the clearance
-- first if you wish to create an 'Labeled' at a higher label than the
-- current clearance.
labelP :: PrivDesc l p => Priv p -> l -> a -> LIO l (Labeled l a)
labelP p l a = do
  withContext "labelP" $ guardAllocP p l
  return $ LabeledTCB l a

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
unlabel :: Label l => Labeled l a -> LIO l a
unlabel = unlabelP noPrivs

-- | Extracts the value of an 'Labeled' just like 'unlabel', but takes a
-- privilege argument to minimize the amount the current label must be
-- raised.  Function will throw 'ClearanceViolation' under the same
-- circumstances as 'unlabel'.
unlabelP :: PrivDesc l p => Priv p -> Labeled l a -> LIO l a
unlabelP p (LabeledTCB l v) = withContext "unlabelP" (taintP p l) >> return v

--
-- Relabel values
--

-- | Relabels a 'Labeled' value to the supplied label if the given
-- privilege privileges permits it.  An exception is thrown unless the
-- following two conditions hold:
--
--   1. The new label must be between the current label and clearance
--      (modulo privileges), as enforced by 'guardAllocP'.
--
--   2. The old label must flow to the new label (again modulo
--      privileges), as enforced by 'canFlowToP'.
--
relabelLabeledP :: PrivDesc l p
                => Priv p -> l -> Labeled l a -> LIO l (Labeled l a)
relabelLabeledP p newl (LabeledTCB oldl v) = do
  withContext "relabelLabeledP" $ guardAllocP p newl
  unless (canFlowToP p oldl newl) $
    labelErrorP "relabelLabeledP" p [oldl, newl]
  return $ LabeledTCB newl v

-- | Raises the label of a 'Labeled' to the 'upperBound' of it's current
-- label and the value supplied.  The label supplied must be bounded by
-- the current label and clearance, though the resulting label may not be
-- if the 'Labeled' is already above the current thread's clearance. If
-- the supplied label is not bounded then @taintLabeled@ will throw an
-- exception (see 'guardAlloc').
taintLabeled :: Label l => l -> Labeled l a -> LIO l (Labeled l a)
taintLabeled = taintLabeledP noPrivs

-- | Same as 'taintLabeled', but uses privileges when comparing the
-- current label to the supplied label. In other words, this function
-- can be used to lower the label of the labeled value by leveraging
-- the supplied privileges.
taintLabeledP :: PrivDesc l p
              => Priv p -> l -> Labeled l a -> LIO l (Labeled l a)
taintLabeledP p l (LabeledTCB lold v) = do
  let lnew = lold `lub` l
  withContext "taintLabeledP" $ guardAllocP p lnew
  return $ LabeledTCB lnew v

{- $functor

Making 'Labeled' an instance of 'Functor' is problematic because:

1. 'fmap' would have type @Labeled l a -> (a -> b) -> Labeled b@ and thus 
    creating /new/ labeled values above the current clearance or below
    the current label would be feasible (given one such value).
2. 'LIO' is polymorphic in the label type and thus 'fmap' would is
   susceptible to /refinement attacks/. Superficially if the label type
   contains an integrity component (see for example "LIO.DCLabel")
   then @fmap (const 3) lv@ would produce a high-integrity labeled @3@
   if @lv@ is a high-integrity labeled value without any any authority
   or /endorsement/.

Similarly, defining an instance of 'Applicative' is an issue.

Instead, we provide 'lFmap' (labeled 'fmap') and 'lAp' that address
the above issues.

-}

-- | Similar to 'fmap', apply function to the 'Labeled' value. The
-- label of the returned value is the least upper bound of the current
-- label and label of the supplied labeled value.
lFmap :: Label l => Labeled l a -> (a -> b) -> LIO l (Labeled l b)
lFmap (LabeledTCB lold v) f = do
  l <- getLabel
  -- Result label is joined with current label
  let lnew = lold `lub` l
  -- `label` checks for clearance violation then labels
  withContext "lFmap" $ label lnew $ f v


-- | Similar to 'ap', apply function (wrapped by 'Labeled') to the
-- labeld value. The label of the returned value is the least upper
-- bound of the current label, label of the supplied labeled value,
-- and label of the supplied function.
lAp :: Label l => Labeled l (a -> b) -> Labeled l a -> LIO l (Labeled l b)
lAp (LabeledTCB lf f) (LabeledTCB la a) = do
  l <- getLabel
  let lnew = l `lub` lf `lub` la
  withContext "lAp" $ label lnew $ f a
