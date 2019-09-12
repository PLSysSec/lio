{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}


module LIO.Label (
  -- * Labels
  -- $Labels
    Label(..)
  -- * Privileges
  -- $Privileges
  , SpeaksFor(..), PrivDesc(..)
  , Priv, privDesc
  -- ** Internal functions
  , isPriv
  -- * Empty privileges
  , NoPrivs(..), noPrivs
  ) where

import safe Data.Monoid ()
import safe Data.Typeable

import LIO.TCB

{- $Labels

Labels are a way of describing who can observe and modify data.
Labels are governed by a partial order, generally pronounced \"can
flow to.\"  In LIO, we write this relation ``canFlowTo``.  In the
literature, it is usually written &#8849;.

At a high level, the purpose of this whole library is to ensure that
data labeled @l1@ may affect data labeled @l2@ only if @l1
``canFlowTo`` l2@.  The 'LIO' monad (see "LIO.Core") ensures this by
keeping track of a /current label/ of the executing thread (accessible
via the 'getLabel' function).  Code may attempt to perform various IO
or memory operations on labeled data.  Touching data may change the
current label and will throw an exception in the event that an
operation would violate information flow restrictions.

The specific invariant maintained by 'LIO' is, first, that labels on
all previously observed data must flow to a thread's current label.
Second, the current label must flow to the labels of any future
objects the thread will be allowed to modify.  Hence, after a thread
with current label @lcur@ observes data labeled @l1@, it must hold
that @l1 ``canFlowTo`` lcur@.  If the thread is later permitted to
modify an object labeled @l2@, it must hold that @lcur ``canFlowTo``
l2@.  By transitivity of the ``canFlowTo`` relation, it holds that @l1
``canFlowTo`` l2@.

-}

-- | This class defines the operations necessary to make a label into
-- a lattice (see <http://en.wikipedia.org/wiki/Lattice_(order)>).
-- 'canFlowTo' partially orders labels.
-- 'lub' and 'glb' compute the least upper bound and greatest lower
-- bound of two labels, respectively.
class (Eq l, Show l, Read l, Typeable l) => Label l where
  -- | Compute the /least upper bound/, or join, of two labels.  When
  -- data carrying two different labels is mixed together in a
  -- document, the @lub@ of the two labels is the lowest safe value
  -- with which to label the result.
  --
  -- More formally, for any two labels @l1@ and @l2@, if @ljoin = l1
  -- \`lub` l2@, it must be that:
  --
  -- * @L_1 ``canFlowTo`` ljoin@,
  --
  -- * @L_2 ``canFlowTo`` ljoin@, and
  --
  -- * There is no label @l /= ljoin@ such that @l1 ``canFlowTo`` l@,
  --   @l2 ``canFlowTo`` l@, and @l ``canFlowTo`` ljoin@.  In other
  --   words @ljoin@ is the least element to which both @l1@ and @l2@
  --   can flow.
  --
  -- When used infix, has fixity:
  --
  -- > infixl 5 `lub`
  lub :: l -> l -> l

  -- | /Greatest lower bound/, or meet, of two labels. For any two
  -- labels @l1@ and @l2@, if @lmeet = l1 \`glb` l2@, it must be
  -- that:
  --
  -- * @lmeet ``canFlowTo`` l1@,
  --
  -- * @lmeet ``canFlowTo`` l2@, and
  --
  -- * There is no label @l /= lmeet@ such that @l ``canFlowTo`` l1@,
  --   @l ``canFlowTo`` l2@, and @lmeet ``canFlowTo`` l@.  In other
  --   words @lmeet@ is the greatest element flowing to both @l1@ and
  --   @l2@.
  --
  -- When used infix, has fixity:
  --
  -- > infixl 5 `glb`
  glb :: l -> l -> l

  -- | /Can-flow-to/ relation (&#8849;). An entity labeled @l1@ should
  -- be allowed to affect an entity @l2@ only if @l1 \`canFlowTo`
  -- l2@. This relation on labels is at least a partial order (see
  -- <https://en.wikipedia.org/wiki/Partially_ordered_set>), and must
  -- satisfy the following laws:
  --
  -- * Reflexivity: @l1 \`canFlowTo` l1@ for any @l1@.
  --
  -- * Antisymmetry: If @l1 \`canFlowTo` l2@ and
  --   @l2 \`canFlowTo` l1@ then @l1 = l2@.
  --
  -- * Transitivity: If @l1 \`canFlowTo` l2@ and
  --   @l2 \`canFlowTo` l3@ then @l1 \`canFlowTo` l3@.
  --
  -- When used infix, has fixity:
  --
  -- > infix 4 `canFlowTo`
  canFlowTo :: l -> l -> Bool

infixl 5 `lub`, `glb`
infix 4 `canFlowTo`

{- $Privileges 

Privileges are objects the possesion of which allows code to bypass
certain label protections.  An instance of class 'PrivDesc' describes
a pre-order (see <http://en.wikipedia.org/wiki/Preorder>) among labels
in which certain unequal labels become equivalent.  A 'Priv' object
containing a 'PrivDesc' instance allows code to make those unequal
labels equivalent for the purposes of many library functions.
Effectively, a 'PrivDesc' instance /describes/ privileges, while a
'Priv' object /embodies/ them.

Any code is free to construct 'PrivDesc' values describing arbitrarily
powerful privileges.  Security is enforced by preventing safe code
from accessing the constructor for 'Priv' (called 'PrivTCB').  Safe
code can construct arbitrary privileges from the 'IO' monad (using
'privInit' in "LIO.Run#v:privInit"), but cannot do so from the 'LIO'
monad.  Starting from existing privileges, safe code can also
'delegate' lesser privileges (see "LIO.Delegate#v:delegate").

Privileges allow you to behave as if @l1 ``canFlowTo`` l2@ even when
that is not the case, but only for certain pairs of labels @l1@ and
@l2@; which pairs depends on the specific privileges.  The process of
allowing data labeled @l1@ to infulence data labeled @l2@ when @(l1
``canFlowTo`` l2) == False@ is known as /downgrading/.

The core privilege function is 'canFlowToP', which performs a more
permissive can-flow-to check by exercising particular privileges (in
the literature this relation is commonly written @&#8849;&#8346;@ for
privileges @p@).  Most core 'LIO' functions have variants ending @...P@
that take a privilege argument to act in a more permissive way.

By convention, all 'PrivDesc' instances should also be instances of
'Monoid', allowing privileges to be combined with 'mappend', though
there is no superclass to enforce this.

-}


-- | Turns privileges into a powerless description of the privileges
-- by unwrapping the 'Priv' newtype.
privDesc :: Priv a -> a
{-# INLINE privDesc #-}
privDesc (PrivTCB a) = a

-- | Uses dynamic typing to return 'True' iff the type of the argument
-- is @'Priv' a@ (for any @a@).  Mostly useful to prevent users from
-- accidentally wrapping 'Priv' objects inside other 'Priv' objects or
-- accidentally including real privileges in an exception.
isPriv :: (Typeable p) => p -> Bool
isPriv p = typeRepTyCon (typeOf p) == privcon
  where privcon = typeRepTyCon $ typeOf noPrivs

-- | Every privilege type must be an instance of 'SpeaksFor', which is
-- a partial order specifying when one privilege value is at least as
-- powerful as another.  If @'canFlowToP' p1 l1 l2@ and @p2
-- `speaksFor` p1@, then it should also be true that @'canFlowToP' p2
-- l1 l2@.
--
-- As a partial order, 'SpeaksFor' should obey the reflexivity,
-- antisymmetry and transitivity laws.  However, if you do not wish to
-- allow delegation of a particular privilege type, you can define
-- @'speaksFor' _ _ = False@ (which violates the reflexivity law, but
-- is reasonable when you don't want the partial order).
class (Typeable p, Show p) => SpeaksFor p where
  -- | @speaksFor p1 p2@ returns 'True' iff @p1@ subsumes all the
  -- privileges of @p2@.  In other words, it is safe for 'delegate' to
  -- hand out @p2@ to a caller who already has @p1@.
  --
  -- Has fixity:
  --
  -- > infix 4 `speaksFor`
  speaksFor :: p -> p -> Bool

infix 4 `speaksFor`

-- | This class represents privilege descriptions, which define a
-- pre-order on labels in which distinct labels become equivalent.
-- The pre-oder implied by a privilege description is specified by the
-- method 'canFlowToP'.  In addition, this this class defines a method
-- 'downgradeP', which is important for finding least labels
-- satisfying a privilege equivalence.
--
-- Minimal complete definition: 'downgradeP'.
--
-- (The 'downgradeP' requirement represents the fact that a generic
-- 'canFlowToP' can be implemented efficiently in terms of
-- 'downgradeP', but not vice-versa.)
class (Label l, SpeaksFor p) => PrivDesc l p where
-- Note: SpeaksFor is a superclass for security reasons.  Were it not
-- a superclass, then if a label format ever failed to define
-- SpeaksFor, or defined it in a different module from the PrivDesc
-- instance, then an attacker could produce an vacuous instance that
-- allows all delegation.

    -- | Privileges are described in terms of a pre-order on labels in
    -- which sets of distinct labels become equivalent.  @downgradeP p
    -- l@ returns the lowest of all labels equivalent to @l@ under
    -- privilege description @p@.
    --
    -- Less formally, @downgradeP p l@ returns a label representing
    -- the furthest you can downgrade data labeled @l@ given
    -- privileges described by @p@.
    downgradeP :: p     -- ^ Privilege description
                  -> l  -- ^ Label to downgrade
                  -> l  -- ^ Lowest label equivelent to input

    -- | @canFlowToP p l1 l2@ determines whether @p@ describes
    -- sufficient privileges to observe data labeled @l1@ and
    -- subsequently write it to an object labeled @l2@.  The function
    -- returns 'True' if and only if either @canFlowTo l1 l2@ or @l1
    -- and l2@ are equivalent under @p@.
    --
    -- The default definition is:
    --
    -- > canFlowToP p l1 l2 = downgradeP p l1 `canFlowTo` l2
    -- 
    -- @canFlowToP@ is a method rather than a function so that it can
    -- be optimized in label-specific ways.  However, custom
    -- definitions should behave identically to the default.
    canFlowToP :: p -> l -> l -> Bool
    canFlowToP p l1 l2 = downgradeP p l1 `canFlowTo` l2

instance (SpeaksFor p) => SpeaksFor (Priv p) where
  {-# INLINE speaksFor #-}
  speaksFor p1 p2 = privDesc p1 `speaksFor` privDesc p2

instance (PrivDesc l p) => PrivDesc l (Priv p) where
  {-# INLINE downgradeP #-}
  downgradeP = downgradeP . privDesc
  {-# INLINE canFlowToP #-}
  canFlowToP = canFlowToP . privDesc

--
-- NoPrivs
--

-- | Generic 'PrivDesc' used to denote the lack of privileges.  Works
-- with any 'Label' type.  This is only a privilege description; a
-- more useful symbol is 'noPrivs', which actually embodies the
-- @NoPrivs@ privilege.
data NoPrivs = NoPrivs deriving (Show, Read, Typeable)

instance SpeaksFor NoPrivs where speaksFor _ _ = True

-- | 'downgradeP' 'NoPrivs' is the identify function.  Hence
-- 'canFlowToP' 'NoPrivs' is the same as 'canFlowTo'.
instance Label l => PrivDesc l NoPrivs where downgradeP _ l = l

instance Semigroup NoPrivs where
  _ <> _ = NoPrivs

instance Monoid NoPrivs where
  mempty = NoPrivs


-- | 'Priv' object corresponding to 'NoPrivs'.
noPrivs :: Priv NoPrivs
noPrivs = PrivTCB NoPrivs
