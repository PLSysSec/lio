{-# LANGUAGE Safe #-}
{- | 

Labels are a way of describing who can observe and modify data.  There
is a partial order, generally pronounced \"can flow to\" on labels.
In LIO we write this partial order ``canFlowTo`` (in the literature it
is usually written as &#8849;).

The idea is that data labeled @L_1@ may affect data labeled @L_2@
only if @L_1@ ``canFlowTo`` @L_2@.  The 'LIO' monad (see "LIO.Core")
keeps track of the current label of the executing code (accessible via
the 'getLabel' function).  Code may attempt to perform various IO or
memory operations on labeled data.  Hence, touching data may change
the current label (or throw an exception if an operation would violate
flow restrictions).

If the current label is @L_cur@, then it is only permissible to read
data labeled @L_r@ if @L_r ``canFlowTo`` L_cur@.  This is sometimes
termed \"no read up\" in the literature; however, because the partial
order allows for incomparable labels (i.e., two labels @L_1@ and @L_2@
such that @not (L_1 ``canFlowTo`` L_2) && not (L_2 ``canFlowTo``
L_1)@), a more appropriate phrasing would be \"read only what can flow
to your label\".  Note that, rather than throw an exception, reading
data will often just increase the current label to ensure that @L_r
``canFlowTo`` L_cur@.  The LIO monad keeps a second label, called the
/clearance/ (accessible via the @getClearance@ function), that
represents the highest value the current thread can raise its label
to. The purpose of clearance is to enforce discretionary access
control: you can set the clearance to a label @L_clear@ so as to prevent
a piece of LIO code from accessing anything above @L_clear@.

Conversely, it is only permissible to modify data labeled @L_w@ when
@L_cur``canFlowTo`` L_w@, a property often cited as \"no write down\",
but more accurately characterized as \"write only what you can flow
to\".  In practice, there are very few IO abstractions (namely,
mutable references) in which it is possible to do a pure write that
doesn't also involve observing some state.  For instance, writing to a
file handle and not getting an exception tells you that the handle is
not closed.  Thus, in practice, the requirement for modifying data
labeled @L_w@ is almost always that @L_cur ``canFlowTo`` L_w@ and @L_w
``canFlowTo`` L_cur@, i.e., @L_cur == L_w@.

Note that higher labels are neither more nor less privileged than
lower ones.  Simply, the higher one's label is, the more things one
can read.  Conversely, the lower one's label, the more things one can
write.  But, because labels are a partial and not a total order, some
data may be completely inaccessible to a particular computation; for
instance, if the current label is @L_cur@, the current clearance is
@C_cur@, and some data is labeled @L_d@, such that @not (L_cur
``canFlowTo`` L_d || L_d ``canFlowTo`` C_cur)@, then the current
thread can neither read nor write the data, at least without invoking
some privilege.

LIO is polymorphic in the label type. It is solely required that every
implementation of a label (usually called a "label format") be an
instance of the 'Label' class. This class provides a generic interface
to labels: they must define the 'canFlowTo' relation, some minimal
element 'bottom', some maximum element 'top', and two binary operators
on how to combine labels: the least upper bound ('lub') and greatest
lower bound ('glb').

Since LIO associates labels with different data types, it is useful to
be able to access the label of such objects (when the label is solely
protected by the current label). To this end, LIO provides the
'LabelOf' type class for which different labeled objects
implementations provide an instance.

-}

module LIO.Label (
  -- * Labels
    Label(..), upperBound, lowerBound
  -- * Accessing label of labeled values
 , LabelOf(..) 
 ) where

import Data.Typeable

-- | This class defines a label format, corresponding to a bounded
-- lattice (see <https://en.wikipedia.org/wiki/Bounded_lattice>).
-- Specifically, it is necessary to define a bottom element
-- 'bottom' (in literature, written as &#8869;), a top element 'top' (in
-- literature, written as &#8868;), a join, or least upper bound, 'lub'
-- (in literature, written as &#8852;), a meet, or greatest lower bound,
-- 'glb' (in literature, written as &#8851;), and of course the
-- can-flow-to partial-order 'canFlowTo' (in literature, written as
-- &#8849;).
class (Eq l, Show l, Typeable l) => Label l where
  -- | /Least/ upper bound, or join, of two labels. For any two labels
  -- @L_1@ and @L_2@, if @L_3 = L_1 \`lub` L_2@, it must be that:
  --
  -- * @L_1 ``canFlowTo`` L_3@,
  --
  -- * @L_2 ``canFlowTo`` L_3@, and
  --
  -- * There is no label @L_4 /= L_3 @ such that
  --   @L_1 ``canFlowTo`` L_4@, @L_2 ``canFlowTo`` L_4@, and
  --   @L_4 ``canFlowTo`` L_3@.  In other words @L_3@ is the least
  --   such element.
  lub :: l -> l -> l
  -- | /Greatest/ lower bound, or meet, of two labels. For any two labels
  -- @L_1@ and @L_2@, if @L_3 = L_1 \`glb` L_2@, it must be that:
  --
  -- * @L_3 ``canFlowTo`` L_1@,
  --
  -- * @L_3 ``canFlowTo`` L_2@, and
  --
  -- * There is no label @L_4 /= L_3@ such that
  --   @L_4 ``canFlowTo`` L_1@, @L_4 ``canFlowTo`` L_1@, and
  --   @L_3 ``canFlowTo`` L_4@.  In other words @L_3@ is the greatest
  --   such element.
  glb :: l -> l -> l
  -- | Can-flow-to relation. An entity labeled @L_1@ should be allowed
  -- to affect an entity @L_2@ only if @L_1 \`canFlowTo` L_2@. This
  -- relation on labels is at least a partial order (see
  -- <https://en.wikipedia.org/wiki/Partially_ordered_set>), and must
  -- satisfy the following rules:
  --
  -- * Reflexivity: @L_1 \`canFlowTo` L_1@ for any @L_1@.
  --
  -- * Antisymmetry: If @L_1 \`canFlowTo` L_2@ and
  --   @L_2 \`canFlowTo` L_1@ then @L_1 = L_2@.
  --
  -- * Transitivity: If @L_1 \`canFlowTo` L_2@ and
  --   @L_2 \`canFlowTo` L_3@ then @L_1 \`canFlowTo` L_3@.
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

-- | Generic class used to get the type of labeled objects. For,
-- instance, if you wish to associate a label with a pure value (as in
-- "LIO.Labeled"), you may create a data type:
-- 
-- > newtype LVal l a = LValTCB (l, a)
-- 
-- Then, you may wish to allow untrusted code to read the label of any
-- @LVal@s but not necessarily the actual value. To do so, simply
-- provide an instance for @LabelOf@:
-- 
-- > instance LabelOf LVal where
-- >   labelOf (LValTCB lv) = fst lv
class LabelOf t where
  -- | Get the label of a type kinded @* -> *@
  labelOf :: Label l => t l a -> l
  
