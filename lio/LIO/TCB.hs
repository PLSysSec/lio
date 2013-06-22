{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{- | 

This module exports 

* The definition of the 'LIO' monad and relevant trusted state
  access/modifying functions.

* Various other types whose constructors are privileged and must be
  hidden from untrusted code.

* Uncatchable exceptions used to pop threads out of the 'LIO' monad
  unconditionally.

* Combinators for executing 'IO' actions within the 'LIO' monad.

The documentation and external, safe 'LIO' interface is provided in
"LIO.Core".

-}

module LIO.TCB (
  -- * LIO monad
    LIOState(..), LIO(..)
  -- ** Accessing internal state
  , getLIOStateTCB, putLIOStateTCB, modifyLIOStateTCB, updateLIOStateTCB 
  -- * Executing IO actions
  , ioTCB
  -- * Privileged constructors
  , Priv(..), Labeled(..), LabelOf(..)
  -- * Uncatchable exception type
  , UncatchableTCB(..), makeCatchable
  -- * Trusted 'Show'
  , ShowTCB(..)
  ) where

import safe Control.Applicative
import safe Control.Exception (Exception(..), SomeException(..))
import safe Control.Monad
import safe Data.Monoid
import safe Data.IORef
import safe Data.Typeable

--
-- LIO Monad
--

-- | Internal state of an 'LIO' computation.
data LIOState l = LIOState { lioLabel     :: !l -- ^ Current label.
                           , lioClearance :: !l -- ^ Current clearance.
                           } deriving (Eq, Show, Read)

-- | The @LIO@ monad is a state monad, with 'IO' as the underlying monad,
-- that carries along a /current label/ ('lioLabel') and /current clearance/
-- ('lioClearance'). The current label imposes restrictions on
-- what the current computation may read and write (e.g., no writes to
-- public channels after reading sensitive data).  Since the current
-- label can be raised to be permissive in what a computation observes,
-- we need a way to prevent certain computations from reading overly
-- sensitive data. This is the role of the current clearance: it imposes
-- an upper bound on the current label.
newtype LIO l a = LIOTCB (IORef (LIOState l) -> IO a) deriving (Typeable)

instance Monad (LIO l) where
  {-# INLINE return #-}
  return = LIOTCB . const . return
  {-# INLINE (>>=) #-}
  (LIOTCB ma) >>= k = LIOTCB $ \s -> do
    a <- ma s
    case k a of LIOTCB mb -> mb s
  fail = LIOTCB . const . fail

instance Functor (LIO l) where
  fmap f (LIOTCB a) = LIOTCB $ \s -> a s >>= return . f
-- fmap typically isn't inlined, so we don't inline our definition,
-- but we do define it in terms of >>= and return (which are inlined)

instance Applicative (LIO l) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

--
-- Internal state
--

-- | Get internal state. This function is not actually unsafe, but
-- to avoid future security bugs we leave all direct access to the
-- internal state to trusted code.
getLIOStateTCB :: LIO l (LIOState l)
{-# INLINE getLIOStateTCB #-}
getLIOStateTCB = LIOTCB readIORef

-- | Set internal state.
putLIOStateTCB :: LIOState l -> LIO l ()
{-# INLINE putLIOStateTCB #-}
putLIOStateTCB s = LIOTCB $ \sp -> writeIORef sp $! s

-- | Update the internal state given some function.
modifyLIOStateTCB :: (LIOState l -> LIOState l) -> LIO l ()
{-# INLINE modifyLIOStateTCB #-}
modifyLIOStateTCB f = do
  s <- getLIOStateTCB
  putLIOStateTCB (f s)

{-# DEPRECATED updateLIOStateTCB "Use modifyLIOStateTCB instead" #-}
updateLIOStateTCB :: (LIOState l -> LIOState l) -> LIO l ()
updateLIOStateTCB = modifyLIOStateTCB

--
-- Executing IO actions
--

-- | Lifts an 'IO' computation into the 'LIO' monad.  Note that
-- exceptions thrown within the 'IO' computation cannot directly be
-- caught within the 'LIO' computation.  Thus, you will generally want to
-- use 'rethrowIoTCB'.
ioTCB :: IO a -> LIO l a
{-# INLINE ioTCB #-}
ioTCB = LIOTCB . const

--
-- Exception handling
--

-- | An uncatchable exception hierarchy use to terminate an untrusted
-- thread.  Wrap the uncatchable exception in 'UncatchableTCB' before
-- throwing it to the thread.  'runLIO' will subsequently unwrap the
-- 'UncatchableTCB' constructor.
--
-- Note this can be circumvented by 'IO.mapException', which should be
-- made unsafe.
data UncatchableTCB = forall e. (Exception e) =>
                      UncatchableTCB e deriving (Typeable)

instance Show UncatchableTCB where
  showsPrec p (UncatchableTCB e) = showsPrec p e

instance Exception UncatchableTCB where
  toException = SomeException
  fromException (SomeException e) = cast e

-- | Simple utility function that strips 'UncatchableTCB' from around an
-- exception.
makeCatchable :: SomeException -> SomeException
makeCatchable e@(SomeException einner) =
  case cast einner of Just (UncatchableTCB enew) -> SomeException enew
                      Nothing                    -> e

--
-- Privileges
--

-- | A newtype wrapper that can be used by trusted code to bless
-- privileges.  The constructor, 'PrivTCB', allows one to mint
-- arbitrary privileges and hence is only exported by the unsafe file
-- "LIO.TCB".  A safe way to create arbitrary privileges is to call
-- 'privInit' (see "LIO.Run#v:privInit") from the 'IO' monad before
-- running your 'LIO' computation.
newtype Priv a = PrivTCB a deriving (Show, Eq, Typeable)

instance Monoid p => Monoid (Priv p) where
  mempty = PrivTCB mempty
  {-# INLINE mappend #-}
  mappend (PrivTCB m1) (PrivTCB m2) = PrivTCB $ m1 `mappend` m2
  {-# INLINE mconcat #-}
  mconcat ps = PrivTCB $ mconcat $ map (\(PrivTCB p) -> p) ps

--
-- Pure labeled values
--

-- | @Labeled l a@ is a value that associates a label of type @l@ with
-- a value of type @a@. Labeled values allow users to label data with
-- a label other than the current label. In an embedded setting this
-- is akin to having first class labeled values. Note that 'Labeled'
-- is an instance of 'LabelOf', which effectively means that the label
-- of a 'Labeled' value is usually just protected by the current
-- label. (Of course if you have a nested labeled value then the label
-- on the inner labeled value's label is the outer label.)
data Labeled l t = LabeledTCB !l t deriving Typeable
-- Note: t cannot be strict if we want things like lFmap.

-- | Trusted 'Show' instance.
instance (Show l, Show a) => ShowTCB (Labeled l a) where
    showTCB (LabeledTCB l a) = show a ++ " {" ++ show l ++ "}"

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
  labelOf :: t l a -> l

instance LabelOf Labeled where
  {-# INLINE labelOf #-}
  labelOf (LabeledTCB l _) = l

--
-- Trusted 'Show'
--

-- | It would be a security issue to make certain objects a member of
-- the 'Show' class, but nonetheless it is useful to be able to
-- examine such objects when debugging.  The 'showTCB' method can be used
-- to examine such objects.
class ShowTCB a where
  showTCB :: a -> String
