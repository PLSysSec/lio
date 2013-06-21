{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | 

Privileges are objects the possesion of which allows code to bypass
certain label protections.  An instance of class 'PrivDesc' describes
a pre-order among labels in which certain unequal labels become
equivalent.  A 'Priv' object containing a 'PrivDesc' instance allows
code to make those unequal labels equivalent for the purposes of many
library functions.  Effectively a 'PrivDesc' instance /describes/
privileges, while a 'Priv' object /embodies/ them.  Security is
enforced by preventing safe code from importing the constructor for
'Priv' (called 'PrivTCB').

Put another way, privileges allow you to behave as if @L_1
``canFlowTo`` L_2@ even when that is not the case, but only for
certain pairs of labels @L_1@ and @L_2@; which pairs depends on the
specific privileges.  The process of allowing data labeled @L_1@ to
infulence data labeled @L_2@ when @(L_1 ``canFlowTo`` L_2) == False@
is known as /downgrading/.

The central function in this module is 'canFlowToP', which performs a
more permissive can-flow-to check by exercising particular privileges
(in literature this relation is commonly written @&#8849;&#8346;@ for
privileges @p@).  Most core 'LIO' function have variants ending @...P@
that take a privilege argument to act in a more permissive way.
'canFlowToP' is defined in terms of the method 'canFlowToPrivDesc',
which performs the same check on a 'PrivDesc' instance.

By convention, all 'PrivDesc' instances should also be instances of
'Monoid', allowing privileges to be combined with 'mappend'.

-}

module LIO.Privs (
  -- * Privilege descriptions
    SpeaksFor(..), delegate
  , PrivDesc(..), downgradeP, canFlowToP, partDowngradeP
  -- * Privileges
  , Priv, privDesc
  , NoPrivs, noPrivs
  -- * Gates
  -- $gateIntro
  , Gate, gate, callGate
  -- ** Gate example
  -- $gateExample
  ) where

import safe Data.Monoid
import safe Data.Typeable

import safe LIO.Label
import LIO.TCB

--
-- No privileges
--

privDesc :: Priv a -> a
{-# INLINE privDesc #-}
privDesc (PrivTCB a) = a

class (Typeable p, Show p) => SpeaksFor p where
  -- | @speaksFor p1 p2@ returns 'True' iff @p1@ subsumes all the
  -- privileges of @p2@.  In other words, it is safe for 'delegate' to
  -- hand out @p2@ to a caller who already has @p1@.
  speaksFor :: p -> p -> Bool
  speaksFor _ _ = False

delegate :: (SpeaksFor p) => Priv p -> p -> Priv p
delegate (PrivTCB p1) p2
  | p1 `speaksFor` p2 = PrivTCB p2
  | otherwise = error $ "Insufficient privileges to delegate " ++ show p2

-- | This class represents privilege descriptions, which define a
-- pre-order on labels in which distinct labels become equivalent.
-- The pre-oder implied by a privilege description is specified by the
-- method 'canFlowToPrivDesc'.  In addition, this this class defines
-- methods 'downgradePrivDesc' and 'partDowngradePrivDesc', which are
-- important for finding least labels satisfying some privilege
-- equivalence.
--
-- Minimal complete definition: 'downgradePrivDesc'.
--
-- (The 'downgradePrivDesc' requirement represents the fact that a
-- generic 'canFlowToPrivDesc' can be implemented efficiently in terms
-- of 'downgradePrivDesc', but not vice-versa.)
class (Label l, SpeaksFor p) => PrivDesc l p where
-- Note: SpeaksFor is a superclass for security reasons.  Were it not
-- a superclass, then if a label format ever failed to define
-- SpeaksFor, or defined it in a different module from the PrivDesc
-- instance, then an attacker could produce an vacuous instance that
-- allows all delegation.
    -- | Privileges are described in terms of a pre-order on labels in
    -- which sets of distinct labels become equivalent.
    -- @downgradePrivDesc p l@ returns the lowest of all labels
    -- equivalent to @l@ under privilege description @p@.
    --
    -- Less formally, @downgradePrivDesc p l@ returns a label
    -- representing the furthest you can downgrade data labeled @l@
    -- given privileges described by @p@.
    --
    -- Yet another way to view this function is that
    -- @downgradePrivDesc p l@ returns the greatest lower bound (under
    -- 'canFlowTo') of the set of all labels @l'@ such that
    -- @'canFlowToPrivDesc' p l' l@.
    downgradePrivDesc :: p  -- ^ Privilege description
                      -> l  -- ^ Label to downgrade
                      -> l  -- ^ Lowest label equivelent to input

    -- | @canFlowToPrivDesc p l1 l2@ determines whether @p@ describes
    -- sufficient privileges to observe data labeled @l1@ and
    -- subsequently write it to an object labeled @l2@.  The function
    -- returns 'True' if and only if either @canFlowTo l1 l2@ or @l1
    -- and l2@ are equivalent under @p@.
    --
    -- The default definition is:
    --
    -- > canFlowToPrivDesc p l1 l2 = downgradePrivDesc p l1 `canFlowTo` l2
    -- 
    -- @canFlowToPrivDesc@ is a method rather than a function so that
    -- it can be optimized in label-specific ways.  However, custom
    -- definitions should behave identically to the default.
    canFlowToPrivDesc :: p -> l -> l -> Bool
    canFlowToPrivDesc p l1 l2 = downgradePrivDesc p l1 `canFlowTo` l2

downgradeP :: PrivDesc l p => Priv p -> l -> l
downgradeP p l = downgradePrivDesc (privDesc p) l

-- | The \"can-flow-to given privileges\" pre-order is used to compare
-- two labels in the presence of privileges.  If @'canFlowToP' p L_1
-- L_2@ holds, then privileges @p@ are sufficient to downgrade data
-- from @L_1@ to @L_2@.  Note that @'canFlowTo' L_1 L_2@ implies
-- @'canFlowToP' p L_1 L_2@ for all @p@, but for some labels and
-- privileges, 'canFlowToP' will hold even where 'canFlowTo' does not.
canFlowToP :: PrivDesc l p => Priv p -> l -> l -> Bool
canFlowToP priv = canFlowToPrivDesc (privDesc priv)

-- | See 'partDowngradeP'. This method uses privilege descriptions
-- instead of privileges.  The default definition is:
--
-- > partDowngradePrivDesc p l1 l2 = downgradePrivDesc p l1 `lub` l2
--
-- @partDowngradePrivDesc@ is a method rather than a function so
-- that it can be optimized in label-specific ways.  However,
-- custom definitions should behave identically to the default.
partDowngradePrivDesc :: (PrivDesc l p) =>
                         p  -- ^ Privilege description
                      -> l  -- ^ Label from which data must flow
                      -> l  -- ^ Goal label
                      -> l  -- ^ Result
partDowngradePrivDesc p l1 l2 = downgradePrivDesc p l1 `lub` l2

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
partDowngradeP :: PrivDesc l p
               => Priv p  -- ^ Privileges
               -> l  -- ^ Label from which data must flow
               -> l  -- ^ Goal label
               -> l  -- ^ Result
partDowngradeP priv = partDowngradePrivDesc (privDesc priv)

{-# DEPRECATED partDowngradePrivDesc, partDowngradeP
  "reformulate in terms of downgradePrivDesc" #-}

-- | Generic privilege type used to denote the lack of privileges.
data NoPrivs = NoPrivs deriving (Show, Read, Typeable)

noPrivs :: Priv NoPrivs
noPrivs = PrivTCB NoPrivs

instance Monoid NoPrivs where
  mempty      = NoPrivs
  mappend _ _ = NoPrivs

instance SpeaksFor NoPrivs where speaksFor _ _ = True

-- | 'downgradePrivDesc' 'NoPrivs' is the identify function.  Hence
-- 'canFlowToPrivDesc' 'NoPrivs' is 'canFlowTo' while
-- 'partDowngradePrivDesc' 'NoPrivs' is 'lub'.
instance Label l => PrivDesc l NoPrivs where downgradePrivDesc _ l = l


{- $gateIntro

LIO provides a basic implementation of /gates/, useful in providing
controlled RPC-like services where the client and service provider are
in mutual distrust. 

A service provider uses 'gate' to create a gate data type @'Gate' d a@
given a computation of type @d -> a@. Here, @d@ is a privilege
description (type variable for an instance of 'PrivDesc').  Gates are
invoked with 'callGate', and as such the service provider has the
guarantee that the client (the caller) owns the privileges
corresponding to the privilege description @d@.  In effect, this
allows a client to \"prove\" to the service provider that they own
certain privileges without entrusting the service with its privileges.
The gate computation can analyze this privilege description before
performing the \"actual\" computation.  The client and server solely
need to trust the implementation of 'callGate'.

-}


-- | A Gate is a lambda abstraction from a privilege description to an
-- arbitrary type @a@. Applying the gate is accomplished with 'callGate'
-- which takes a privilege argument that is converted to a description
-- before invoking the gate computation.
newtype Gate d a = GateTCB (d -> a) deriving Typeable
-- Note GateTCB is trusted by convention.  Anyone with access to the
-- symbol can call any gate while claiming arbitrary privileges.  In
-- the absence of gates, however, GateTCB doesn't provide any
-- particular privileges.

-- | Create a gate given a computation from a privilege description.
-- Note that because of currying type 'a' may itself be a function
-- type and thus gates can take arguments in addition to the privilege
-- descriptoin.
gate :: (d -> a)  -- ^ Gate computation
     -> Gate d a
{-# INLINE gate #-}
gate = GateTCB

-- | Given a gate and privilege, execute the gate computation.  It is
-- important to note that @callGate@ invokes the gate computation with
-- the privilege description and /NOT/ the privilege itself.
--
-- Note that, in general, code should /not/ provide privileges to
-- functions other than @callGate@ when wishing to call a gate. This
-- function is provided by LIO since it can be easily inspected by
-- both the gate creator and caller to be doing the \"right\" thing:
-- provide the privilege description corresponding to the supplied
-- privilege as \"proof\" without explicitly passing in the privilege.
-- 
callGate :: Gate p a -- ^ Gate
         -> Priv p   -- ^ Privilege used as proof-of-ownership
         -> a
{-# INLINE callGate #-}
callGate (GateTCB g) = g . privDesc

{- $gateExample

This example uses "LIO.DCLabel" to demonstrate the use of gates.  The
service provider provides @addGate@ which adds two integers if the
gate is called by a piece of code that owns the \"Alice\" or \"Bob\"
principals. Otherwise, it simply returns @Nothing@.

> import LIO
> import LIO.DCLabel
> 
> import LIO.Privs.TCB (mintTCB)
> 
> 
> -- | Add two numbers if the computation is invoked by Alice or Bob.
> addGate :: DCGate (Int -> Int -> Maybe Int)
> addGate = gate $ \pd a b ->
>   if pd `elem` (dcPrivDesc `map` ["Alice", "Bob"])
>     then Just $ a + b
>     else Nothing
> 
> 
> alice, bob, clark :: DCPriv
> alice = PrivTCB . dcPrivDesc $ "Alice"
> bob   = PrivTCB . dcPrivDesc $ "Bob"
> clark = PrivTCB . dcPrivDesc $ "Clark"
> 
> main = putStrLn . show $ 
>   [ callGate addGate alice 1 2 -- Just 3
>   , callGate addGate bob   3 4 -- Just 7
>   , callGate addGate clark 5 6 -- Nothing
>   ]


-}
