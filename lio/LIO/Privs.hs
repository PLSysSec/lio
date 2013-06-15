{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 

Privileges are objects the possesion of which allows code to bypass
some label protections.  An in instance of class 'PrivDesc' describes
a pre-order among labels in which certain unequal labels become
equivalent.  When wrapped in a 'Priv' type (whose constructor is
private) a 'PrivDesc' allows code to treat those labels as equivalent.

Put another way, privileges represent the ability to bypass the
protection of certain labels.  Specifically, privilege allows you to
behave as if @L_1 ``canFlowTo`` L_2@ even when that is not the case.
The process of making data labeled @L_1@ affect data labeled @L_2@
when @not (L_1 ``canFlowTo`` L_2)@ is called /downgrading/.

The basic method of the 'PrivDesc' class is 'canFlowToP', which
performs a more permissive can-flow-to check by exercising particular
privileges (in literature this relation is a pre-order, commonly
written as &#8849;&#8346;).  Almost all 'LIO' operations have variants
ending @...P@ that take a privilege argument to act in a more
permissive way.

By convention, all 'PrivDesc' instances are also be instances of
'Monoid', allowing privileges to be combined with 'mappend'.  The
creation of 'PrivDesc' values is specific to the particular label type
in use; the method used is 'mintTCB', but the arguments depend on the
particular label type.

-}

module LIO.Privs (
  -- * Privilege descriptions
    PrivDesc(..), canFlowToP, partDowngradeP
  -- * Privileges
  , Priv, privDesc
  , NoPrivs, noPrivs
  -- * Gates
  -- $gateIntro
  , Gate, gate, callGate
  -- ** Gate example
  -- $gateExample
  ) where

import Data.Monoid
import LIO.Label
import LIO.TCB

--
-- No privileges
--

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


-- | Generic privilege type used to denote the lack of privileges.
data NoPrivs = NoPrivs deriving (Show, Read)

noPrivs :: Priv NoPrivs
noPrivs = MintTCB NoPrivs

instance Monoid NoPrivs where
  mempty      = NoPrivs
  mappend _ _ = NoPrivs

-- | With lack of privileges, 'canFlowToP' is simply 'canFlowTo', and
-- 'partDowngradeP' is the least 'upperBound'.
instance Label l => PrivDesc l NoPrivs where
  canFlowToPrivDesc _ l1 l2    = l1 `canFlowTo` l2
  partDowngradePrivDesc _ l lg = l `lub` lg


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
newtype Gate d a = GateTCB (d -> a)
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
> alice = mintTCB . dcPrivDesc $ "Alice"
> bob   = mintTCB . dcPrivDesc $ "Bob"
> clark = mintTCB . dcPrivDesc $ "Clark"
> 
> main = putStrLn . show $ 
>   [ callGate addGate alice 1 2 -- Just 3
>   , callGate addGate bob   3 4 -- Just 7
>   , callGate addGate clark 5 6 -- Nothing
>   ]


-}
