{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE SafeImports #-}
#endif
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module implements the core of the Labeled IO library for
-- information flow control in Haskell.  It provides a monad, 'LIO',
-- that is intended to be used as a replacement for the IO monad in
-- untrusted code.  The idea is for untrusted code to provide a
-- computation in the 'LIO' monad, which trusted code can then safely
-- execute through the 'evalLIO' function.  (Though usually a wrapper
-- function is employed depending on the type of labels used by an
-- application.  For example, with "LIO.DCLabel", you would use
-- 'evalDC' to execute an untrusted computation. There are also
-- abbreviations for the 'LIO' monad type of a particular
-- label--for instance 'DC'.
--
-- A data structure 'Labeled' (labeled value) protects access to pure
-- values.  Without the appropriate privileges, one cannot produce a
-- pure value that depends on a secret 'Labeled', or conversely produce a
-- high-integrity 'Labeled' based on pure data.  The function 'toLabeled'
-- allows one to seal off the results of an LIO computation inside an
-- 'Labeled' without tainting the current flow of execution.  'unlabel'
-- conversely allows one to use the value stored within a 'Labeled'.
--
-- We note that using 'toLabeled' is /not/ safe with respect to
-- the termination covert channel. Specifically, LIO with 'toLabeled'
-- is only termination-insensitive non-interfering. For a
-- termination-sensitive interface you must use "LIO.Concurrent".
-- 
-- Any code that imports this module is part of the
-- /Trusted Computing Base/ (TCB) of the system.  Hence, untrusted
-- code must be prevented from importing this module.  The exported
-- symbols ending @...TCB@ can be used to violate label protections
-- even from within pure code or the LIO Monad.  A safe subset of
-- these symbols is exported by the "LIO.Safe" module, which is how
-- untrusted code should access the core label functionality.
-- ("LIO.Safe" is also re-exported by "LIO", the main gateway to
-- this library.)
--
module LIO.TCB (-- * Basic Label Functions
                -- $labels
                Label(..)
                -- * Basic Privilige Functions
                -- $privs
               , Priv(..), NoPrivs(..)
               -- * Labeled IO Monad (LIO)
               -- $LIO
               , LIO, LabelState
               , evalLIO, runLIO
               , newState
               , getLabel, setLabelP
               , getClearance, lowerClr, lowerClrP
               , withClearance
               -- * Labeled Values
               , Labeled
               , labelOf
               , label, labelP
               , unlabel, unlabelP
               , taintLabeled
               , toLabeled, toLabeledP, discard, discardP
               -- ** LIO Guards
               -- $guards
               , taint, taintP
               , wguard, wguardP
               , aguard, aguardP
               -- * Labeled Exceptions
               -- $lexception
               -- ** Exception type thrown by LIO library
               , LabelFault(..)
               -- ** Throwing and Catching Labeled Exceptions
               -- $throw
               , LabeledException(..)
               , MonadCatch(..)
               , catchP, handleP
               , onExceptionP, bracketP
               , evaluate
               -- * Unsafe (TCB) Operations
               -- ** Basic Label Functions
               , PrivTCB, MintTCB(..)
               -- ** Labeled IO Monad (LIO)
               , LIOstate(..)
               , getTCB, putTCB
               , setLabelTCB, lowerClrTCB
               -- ** Labeled Values
               , ShowTCB(..), ReadTCB(..)
               , labelTCB, unlabelTCB
               -- ** Labeled Exceptions
               , catchTCB, OnExceptionTCB(..)
               , ioTCB, rtioTCB
               ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
import safe Prelude hiding (catch)
import Control.Monad.Error
-- #warning "Did not safely import Control.Monad.Error"
import Control.Monad.State.Lazy hiding (put, get)
-- #warning "Did not safely import Control.Monad.State.Lazy"
import safe Control.Exception hiding (catch, handle, throw, throwIO,
                                 onException, block, unblock, evaluate)
import safe qualified Control.Exception as E
import safe Data.Monoid
import safe Data.Typeable
import safe Data.Functor
import safe Control.Applicative
import safe Text.Read (minPrec)
import safe LIO.MonadCatch

#else
import Prelude hiding (catch)
import Control.Monad.Error
import Control.Monad.State.Lazy hiding (put, get)
import qualified Control.Exception as E
import Data.Monoid
import Data.Typeable
import Data.Functor
import Control.Applicative
import Text.Read (minPrec)
import LIO.MonadCatch

#endif

---------------------------------------------------------------------
-- Basic label functions --------------------------------------------
---------------------------------------------------------------------

{- $labels

Labels are a way of describing who can observe and modify data.  There
is a partial order, generally pronounced \"can flow to\" on labels.
In Haskell we write this partial order ``leq`` (in the literature it
is usually written as &#8849;).

The idea is that data labeled @L_1@ should affect data labeled
@L_2@ only if @L_1@ ``leq`` @L_2@, (i.e., @L_1@ /can flow to/
@L_2@).  The 'LIO' monad keeps track of the current label of the
executing code (accessible via the 'getLabel' function).  Code may
attempt to perform various IO or memory operations on labeled data.
Touching data may change the current label (or throw an exception
if an operation would violate can-flow-to restrictions).

If the current label is @L_cur@, then it is only permissible to
read data labeled @L_r@ if @L_r ``leq`` L_cur@.  This is sometimes
termed \"no read up\" in the literature; however, because the partial
order allows for incomparable labels (i.e., two labels @L_1@ and
@L_2@ such that @not (L_1 ``leq`` L_2) && not (L_2 ``leq`` L_1)@),
a more appropriate phrasing would be \"read only what can flow to
your label\".  Note that, rather than throw an exception, reading
data will often just increase the current label to ensure that
@L_r ``leq`` L_cur@.  The LIO monad keeps a second label, called
the /clearance/ (see 'getClearance'), that represents the highest
value the current thread can raise its label to. The purpose of
clearance is to enforce of discretionary access control: you can
set the clearance to a label @L_clear@ as to prevent a piece of
LIO code from reading anything above @L_clear@.

Conversely, it is only permissible to modify data labeled @L_w@
when @L_cur``leq`` L_w@, a property often cited as \"no write
down\", but more accurately characterized as \"write only what you
can flow to\".  In practice, there are very few IO abstractions
(namely, mutable references) in which it is possible to do a pure
write that doesn't also involve observing some state.  For instance,
writing to a file handle and not getting an exception tells you that
the handle is not closed.  Thus, in practice, the requirement for
modifying data labeled @L_w@ is almost always that @L_cur == L_w@.

Note that higher labels are neither more nor less privileged than
lower ones.  Simply, the higher one's label is, the more things one
can read.  Conversely, the lower one's label, the more things one can
write.  But, because labels are a partial and not a total order, 
some data may be completely inaccessible to a particular computation;
for instance, if the current label is @L_cur@, the current clearance
  is @C_cur@, and some data is labeled @Ld_@, such that @not (L_cur
``leq`` L_d || L_d ``leq`` C_cur)@, then the current thread can
neither read nor write the data, at least without invoking some
privilege.
-}

{- | This class defines a label format, corresponding to a bounded
lattice. Specifically, it is necessary to define a bottom element
'lbot' (in literature, written as &#8869;), a top element 'ltop'
(in literature, written as &#8868;), a join, or least upper bound,
'lub' (in literature, written as &#8852;), a meet, or greatest lower
bound, 'glb' (in literature, written as &#8851;), and of course the
can-flow-to partial-order 'leq' (in literature, written as &#8849;).
-}
class (Eq a, Show a, Read a, Typeable a) => Label a where
    -- | Bottom
    lbot :: a
    -- | Top
    ltop :: a
    -- | Least upper bound (join) of two labels
    lub :: a -> a -> a
    -- | Greatest lower bound (meet) of two labels
    glb :: a -> a -> a
    -- | Can-flow-to relation
    leq :: a -> a -> Bool

---------------------------------------------------------------------
-- Basic privilege functions ----------------------------------------
---------------------------------------------------------------------

{- $privs

Privilege comes from a separate class called 'Priv', representing the
ability to bypass the protection of certain labels.  Essentially,
privilege allows you to behave as if @L_1 ``leq`` L_2@ even when
that is not the case.  The process of making data labeled @L_1@
affect data labeled @L_2@ when @not (L_1 ``leq`` L_2)@ is called
/downgrading/.

The basic method of the 'Priv' object is 'leqp', which performs the
more permissive can-flow-to check in the presence of particular
privileges (in literature this relation is a pre-order, commonly
written as &#8849;&#8346;).  Many 'LIO' operations have variants
ending @...P@ that take a privilege argument to act in a more
permissive way.  All 'Priv' types are monoids, and so can be combined
with 'mappend'.

How to create 'Priv' objects is specific to the particular label
type in use.  The method used is 'mintTCB', but the arguments depend
on the particular label type.  (Of course, the symbol 'mintTCB'
must not be available to untrusted code.)

-}


-- | @PrivTCB@ is a method-less class whose only purpose is to be
-- unavailable to unprivileged code.  Since @(PrivTCB t) =>@ is in the
-- context of class 'Priv' and unprivileged code cannot create new
-- instances of the @PrivTCB@ class, this ensures unprivileged code
-- cannot create new instances of the 'Priv' class either, even though
-- the symbol 'Priv' is exported by "LIO.Base" and visible to
-- untrusted code.
class PrivTCB t where

-- | This class defines privileges and the more-permissive relation
-- ('leqp') on labels using privileges. Additionally, it defines
-- 'lostar' which is used to compute the smallest difference between
-- two labels given a set of privilege.
class (Label l, Monoid p, PrivTCB p) => Priv l p where
    -- | The \"can-flow-to given privileges\" pre-order used to
    -- compare two labels in the presence of privileges.
    -- If @'leqp' p L_1 L_2@ holds, then privileges @p@ are sufficient to
    -- downgrade data from @L_1@ to @L_2@.  Note that @'leq' L_1 L_2@
    -- implies @'leq' p L_1 L_2@ for all @p@, but for some labels and
    -- privileges, 'leqp' will hold even where 'leq' does not.
    leqp :: p -> l -> l -> Bool
    leqp p a b = lostar p a b `leq` b

    -- | Roughly speaking, @L_r = lostar p L L_g@ computes how close
    -- one can come to downgrading data labeled @L@ to the goal label
    -- @L_g@, given privileges @p@.  When @p == 'NoPrivs'@, the resulting
    -- label @L_r == L ``lub``L_g@.  If @p@ contains all possible privileges,
    -- then @L_r == L_g@.
    --
    -- More specifically, @L_r@ is the greatest lower bound of the
    -- set of all labels @L_l@ satisfying:
    --
    --   1. @ L_g &#8849; L_l@, and
    --
    --   2. @ L &#8849;&#8346; L_l@.
    --
    -- Operationally, @lostar@ captures the minimum change required to
    -- the current label when viewing data labeled @L_l@.  A common
    -- pattern is to use the result of 'getLabel' as @L_g@ (i.e.,
    -- the goal is to use privileges @p@ to avoid changing the label
    -- at all), and then compute @L_r@ based on the label of data
    -- the code is about to observe.  For example, 'taintP' could be
    -- implemented as:
    --
    -- @
    --    taintP p l = do lcurrent <- 'getLabel'
    --                    'taint' (lostar p l lcurrent)
    -- @
    lostar :: p                 -- ^ Privileges
           -> l                 -- ^ Label from which data must flow
           -> l                 -- ^ Goal label
           -> l                 -- ^ Result

class MintTCB t i where
    -- |A function that mints new objects (such as instances of
    -- 'Priv') in a way that only privileged code should be allowed to
    -- do.  Because the MintTCB method is only available to
    -- priviledged code, other modules imported by unpriviledged code
    -- can define instances of mintTCB.
    mintTCB :: i -> t

-- |A generic 'Priv' instance that works for all 'Label's and confers
-- no downgrading privileges.
data NoPrivs = NoPrivs
instance PrivTCB NoPrivs
instance Monoid NoPrivs where
    mempty      = NoPrivs
    mappend _ _ = NoPrivs
instance (Label l) => Priv l NoPrivs where
    leqp _ a b      = leq a b
    lostar _ l goal = lub l goal

---------------------------------------------------------------------
-- Labeled IO -------------------------------------------------------
---------------------------------------------------------------------

-- $LIO
-- 
-- The 'LIO' monad is a wrapper around 'IO' that keeps track of the
-- current label and clearance.  It is possible to raise one's label
-- or lower one's clearance without privilege, but moving in the other
-- direction requires appropriate privilege.
--
-- 'LIO' is parameterized by two types.  The first is the particular
-- label type.  The second type is state specific to and 
-- functionally determined by the label type.
-- Trusted label implementation code can use 'getTCB' and 'putTCB' to
-- get and set the label state.

-- | Empty class used to specify the functional dependency between a
-- label and it state.
class Label l => LabelState l s | l -> s where


-- | Internal state of an 'LIO' computation.
data LIOstate l s = LIOstate { labelState :: s -- label-specific state
                             , lioL :: l -- current label
                             , lioC :: l -- current clearance
                             }

-- | LIO monad is a State monad transformer with IO as the underlying
-- monad.
newtype LIO l s a = LIO (StateT (LIOstate l s) IO a)
    deriving (Functor, Applicative, Monad)

get :: (LabelState l s) => LIO l s (LIOstate l s)
get = mkLIO $ \s -> return (s, s)

put :: (LabelState l s) => LIOstate l s -> LIO l s ()
put s = mkLIO $ \_ -> return (() , s)

-- | Returns label-specific state of the 'LIO' monad.  This is the
-- data specified as the second argument of 'evalLIO', whose type is
-- @s@ in the monad @LIO l s@.
getTCB :: (LabelState l s) => LIO l s s
getTCB = labelState <$> get

-- | Sets the label-specific state of the 'LIO' monad.  See 'getTCB'.
putTCB :: (LabelState l s) => s -> LIO l s ()
putTCB ls = get >>= put . update
    where update s = s { labelState = ls }

-- | Generate a fresh state to pass 'runLIO' when invoking it for the
-- first time. The current label is set to 'lbot' and the current
-- clearance is set to 'ltop'.
newState :: (LabelState l s) => s -> LIOstate l s
newState s = LIOstate { labelState = s , lioL = lbot , lioC = ltop }

mkLIO :: (LabelState l s) => (LIOstate l s -> IO (a, LIOstate l s)) -> LIO l s a
mkLIO = LIO . StateT

unLIO :: (LabelState l s) => LIO l s a -> LIOstate l s -> IO (a, LIOstate l s)
unLIO (LIO m) = runStateT m

-- | Execute an LIO action. The label on exceptions are removed.
-- See 'evalLIO'.
runLIO :: forall l s a. (LabelState l s)
       => LIO l s a -> LIOstate l s -> IO (a, LIOstate l s)
runLIO m s = unLIO m s `catch` (throwIO . delabel)
    where delabel :: LabeledException l -> SomeException
          delabel (LabeledExceptionTCB _ e) = e

-- | Produces an 'IO' computation that will execute a particular 'LIO'
-- computation.  Because untrusted code cannot execute 'IO'
-- computations, this function should only be useful within trusted
-- code.  No harm is done from exposing the @evalLIO@ symbol to
-- untrusted code.  (In general, untrusted code is free to produce
-- 'IO' computations--it just can't execute them without access to
-- 'ioTCB'.)
evalLIO :: (LabelState l s) =>
           LIO l s a -- ^ The LIO computation to execute
        -> s         -- ^ Initial value of label-specific state
        -> IO (a, l) -- ^ IO computation that will execute first argument
evalLIO m s = do (a, ls) <- runLIO m (newState s)
                 return (a, lioL ls)

-- | Returns the current value of the thread's label.
getLabel :: (LabelState l s) => LIO l s l
getLabel = lioL <$> get

-- | Returns the current value of the thread's clearance.
getClearance :: (LabelState l s) => LIO l s l
getClearance = lioC <$> get

-- | If the current label is @oldLabel@ and the current clearance is
-- @clearance@, this function allows code to raise the label to any
-- value @newLabel@ such that
-- @oldLabel ``leq`` newLabel && newLabel ``leq`` clearance@.
-- Note that there is no @setLabel@ variant without the @...P@ because
-- the 'taint' function provides essentially the same functionality
-- that @setLabel@ would.
setLabelP :: (Priv l p, LabelState l s) => p -> l -> LIO l s ()
setLabelP p l = do s <- get
                   if leqp p (lioL s) l
                     then put s { lioL = l }
                     else throwIO LerrPriv

-- | Set the current label to anything, with no security check.
setLabelTCB :: (LabelState l s) => l -> LIO l s ()
setLabelTCB l = do s <- get
                   if l `leq` lioC s
                      then put s { lioL = l }
                      else throwIO LerrClearance

-- |Reduce the current clearance.  One cannot raise the current label
-- or create object with labels higher than the current clearance.
lowerClr :: (LabelState l s) => l -> LIO l s ()
lowerClr l = get >>= doit
    where doit s | not $ l `leq` lioC s = throwIO LerrClearance
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = put s { lioC = l }

-- |Raise the current clearance (undoing the effects of 'lowerClr').
-- This requires privileges.
lowerClrP :: (Priv l p, LabelState l s) => p -> l -> LIO l s ()
lowerClrP p l = get >>= doit
    where doit s | not $ leqp p l $ lioC s = throwIO LerrPriv
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = put s { lioC = l }

-- | Set the current clearance to anything, with no security check.
lowerClrTCB :: (LabelState l s) => l -> LIO l s ()
lowerClrTCB l = get >>= doit
    where doit s | not $ lioL s `leq` l =
            throwIO $ LerrInval ("Cannot lower the current clearance"
                                 ++ " below current label.")
                 | otherwise            = put s { lioC = l }

-- | Lowers the clearance of a computation, then restores the
-- clearance to its previous value.  Useful to wrap around a
-- computation if you want to be sure you can catch exceptions thrown
-- by it.  Also useful to wrap around 'toLabeled' to ensure that the
-- computation does not access data exceeding a particular label.  If
-- @withClearance@ is given a label that can't flow to the current
-- clearance, then the clearance is lowered to the greatest lower
-- bound of the label supplied and the current clearance.
--
-- Note that if the computation inside @withClearance@ acquires any
-- 'Priv's, it may still be able to raise its clearance above the
-- supplied argument using 'lowerClrP'.
withClearance :: (LabelState l s) => l -> LIO l s a -> LIO l s a
withClearance l m = do
  s <- get
  put s { lioC = l `glb` lioC s }
  a <- m
  put s { lioC = lioC s }
  return a

-- | Lifts an 'IO' computation into the 'LIO' monad.  Note that
-- exceptions thrown within the 'IO' computation cannot directly be
-- caught within the 'LIO' computation.  Thus, you will generally
-- want to use 'rtioTCB' instead of 'ioTCB'.
ioTCB :: (LabelState l s) => IO a -> LIO l s a
ioTCB a = mkLIO $ \s -> do
  r <- a
  return (r, s)

-- | Lifts an 'IO' computation into the 'LIO' monad.  If the 'IO'
-- computation throws an exception, it labels the exception with the
-- current label so that the exception can be caught with 'catch' or
-- 'catchP'.  This function's name stands for \"re-throw io\".
rtioTCB :: (LabelState l s) => IO a -> LIO l s a
rtioTCB io = do
  l <- getLabel
  ioTCB $ io `catch` (throwIO . (LabeledExceptionTCB l))

---------------------------------------------------------------------
-- Labeled value ----------------------------------------------------
---------------------------------------------------------------------

-- | @Labeled@ is a type representing labeled data.  
data Labeled l t = LabeledTCB l t
  deriving (Functor)

-- | Returns label of a 'Labeled' type.
labelOf :: Label l => Labeled l a -> l
labelOf (LabeledTCB l _) = l

-- | Function to construct a 'Labeled' from a label and pure value.  If
-- the current label is @lcurrent@ and the current clearance is
-- @ccurrent@, then the label @l@ specified must satisfy
-- @lcurrent ``leq`` l && l ``leq`` ccurrent@.
label :: (LabelState l s) => l -> a -> LIO l s (Labeled l a)
label = labelP NoPrivs

-- | Constructs a 'Labeled' using privilege to allow the `Labeled`'s label
-- to be below the current label.  If the current label is @lcurrent@
-- and the current clearance is @ccurrent@, then the privilege @p@ and
-- label @l@ specified must satisfy
-- @(leqp p lcurrent l) && l ``leq`` ccurrent@.
-- Note that privilege is not used to bypass the clearance.  You must
-- use 'lowerClrP' to raise the clearance first if you wish to
-- create an 'Labeled' at a higher label than the current clearance.
labelP :: (Priv l p, LabelState l s) => p -> l -> a -> LIO l s (Labeled l a)
labelP p l a = get >>= doit
    where doit s | not $ l `leq` lioC s    = throwIO LerrClearance
                 | not $ leqp p (lioL s) l = throwIO LerrLow
                 | otherwise               = return $ LabeledTCB l a

-- | Trusted constructor that creates labeled values.
labelTCB :: Label l => l -> a -> Labeled l a
labelTCB l a = LabeledTCB l a

-- | Within the 'LIO' monad, this function takes a 'Labeled' and returns
-- the value.  Thus, in the 'LIO' monad one can say:
--
-- > x <- unlabel (xv :: Labeled SomeLabelType Int)
--
-- And now it is possible to use the value of @x@, which is the pure
-- value of what was stored in @xv@.  Of course, @unlabel@ also raises
-- the current label.  If raising the label would exceed the current
-- clearance, then @unlabel@ throws 'LerrClearance'.
-- However, you can use 'labelOf' to check if 'unlabel' will suceed without
-- throwing an exception.
unlabel :: (LabelState l s) => Labeled l a -> LIO l s a
unlabel = unlabelP NoPrivs

-- | Extracts the value of an 'Labeled' just like 'unlabel', but takes a
-- privilege argument to minimize the amount the current label must be
-- raised.  Will still throw 'LerrClearance' under the same
-- circumstances as 'unlabel'.
unlabelP :: (Priv l p, LabelState l s) => p -> Labeled l a -> LIO l s a
unlabelP p (LabeledTCB la a) = taintP p la >> return a

-- | Extracts the value from an 'Labeled', discarding the label and any
-- protection.
unlabelTCB :: Label l => Labeled l a -> a
unlabelTCB (LabeledTCB _ a) = a

-- | Raises the label of a 'Labeled' to the 'lub' of it's current label
-- and the value supplied.  The label supplied must be less than the
-- current clarance, though the resulting label may not be if the
-- 'Labeled' is already above the current thread's clearance.
taintLabeled :: (LabelState l s) => l -> Labeled l a -> LIO l s (Labeled l a)
taintLabeled l (LabeledTCB la a) = do
  aguard l
  return $ LabeledTCB (lub l la) a


-- |@toLabeled@ is the dual of @unlabel@.  It allows one to invoke
-- computations that would raise the current label, but without
-- actually raising the label.  Instead, the result of the computation
-- is packaged into a 'Labeled' with a supplied label.
-- Thus, to get at the result of the
-- computation one will have to call 'unlabel' and raise the label, but
-- this can be postponed, or done inside some other call to 'toLabeled'.
-- This suggestst that the provided label must be above the current
-- label and below the current clearance.
--
-- Note that @toLabeled@ always restores the clearance to whatever it was
-- when it was invoked, regardless of what occured in the computation
-- producing the value of the 'Labeled'. 
-- This higlights one main use of clearance: to ensure that a @Labeled@
-- computed does not exceed a particular label.
--
-- WARNING: toLabeled is susceptible to termination attacks.
--
toLabeled :: (LabelState l s) => l -> LIO l s a -> LIO l s (Labeled l a)
toLabeled = toLabeledP NoPrivs
{-# WARNING toLabeled "toLabeled is susceptible to termination attacks" #-}

-- | Same as 'toLabeled' but allows one to supply a privilege object
-- when comparing the initial and final label of the computation.
--
-- WARNING: toLabeledP is susceptible to termination attacks.
--
toLabeledP :: (Priv l p, LabelState l s)
           => p -> l -> LIO l s a -> LIO l s (Labeled l a)
toLabeledP p l m = do
  aguardP p l
  save_s <- get
  a <- m
  s <- get
  put s { lioL = lioL save_s, lioC = lioC save_s}
  unless (leqp p (lioL s) l) $ throwIO LerrLow -- l is too low
  return $ LabeledTCB l a
{-# WARNING toLabeledP "toLabeledP is susceptible to termination attacks" #-}

-- | Executes a computation that would raise the current label, but
-- discards the result so as to keep the label the same.  Used when
-- one only cares about the side effects of a computation.  For
-- instance, if @log_handle@ is an 'LHandle' with a high label, one
-- can execute
--
-- @
--   discard ltop $ 'hputStrLn' log_handle \"Log message\"
-- @
--
-- to create a log message without affecting the current label.  (Of
-- course, if @log_handle@ is closed and this throws an exception, it
-- may not be possible to catch the exception within the 'LIO' monad
-- without sufficient privileges--see 'catchP'.)
--
-- WARNING: discard is susceptible to termination attacks.
--
discard :: (LabelState l s) =>  l -> LIO l s a -> LIO l s ()
discard = discardP NoPrivs
{-# WARNING discard "discard is susceptible to termination attacks" #-}

-- | Same as 'discard', but uses privileges when comparing initial and
-- final label of the computation.
discardP :: (Priv l p, LabelState l s) => p -> l -> LIO l s a -> LIO l s ()
discardP p l m = toLabeledP p l m >> return ()
{-# WARNING discardP "discardP is susceptible to termination attacks" #-}



-- | It would be a security issue to make certain objects a member of
-- the 'Show' class, but nonetheless it is useful to be able to
-- examine such objects when debugging.  The 'showTCB' method can be used
-- to examine such objects.
class ShowTCB a where
    showTCB :: a -> String

instance (Label l, Show a) => ShowTCB (Labeled l a) where
    showTCB (LabeledTCB l t) = show t ++ " {" ++ show l ++ "}"

-- | It is useful to have the dual of 'ShowTCB', @ReadTCB@, that allows
-- for the reading of 'Labeled's that were written using 'showTCB'. Only
-- @readTCB@ (corresponding to 'read') and @readsPrecTCB@ (corresponding
-- to 'readsPrec') are implemented.
class ReadTCB a where
  readsPrecTCB :: Int -> ReadS a
  readTCB :: String -> a
  readTCB str = check $ readsPrecTCB minPrec str
    where check []                          = error "readTCB: no parse"
          check [(x,rst)] | all (==' ') rst = x
                         | otherwise        = error "readTCB: no parse"
          check _                           = error "readTCB: ambiguous parse"

instance (Label l, Read l, Read a) => ReadTCB (Labeled l a) where
  readsPrecTCB _ str = do (val, str1) <- reads str
                          ("{", str2) <- lex str1
                          (lab, str3) <- reads str2
                          ("}", rest) <- lex str3
                          return (labelTCB lab val, rest)

---------------------------------------------------------------------
-- LIO guards -------------------------------------------------------
---------------------------------------------------------------------

{- $guards

   Guards are used by privileged code to check that the invoking,
   unprivileged code has access to particular data.  If the current
   label is @lcurrent@ and the current clearance is @ccurrent@, then
   the following checks should be performed when accessing data
   labeled @ldata@:

   * When /reading/ an object labeled @ldata@, it must be the case
     that @ldata ``leq`` lcurrent@.  This check is performed by the
     'taint' function, so named becuase it \"taints\" the current LIO
     context by raising @lcurrent@ until @ldata ``leq`` lcurrent@.
     (Specifically, it does this by computing the least upper bound of
     the two labels with the 'lub' method of the 'Label' class.)
     However, if after doing this it would be the case that
     @not (lcurrent ``leq`` ccurrent)@, then 'taint' throws exception
     'LerrClearance' rather than raising the current label.

   * When /writing/ an object, it should be the case that @ldata
     ``leq`` lcurrent && lcurrent ``leq`` ldata@.  (As stated, this is
     the same as saying @ldata == lcurrent@, but the two are different
     when using 'leqp' instead of 'leq'.)  This is ensured by the
     'wguard' (write guard) function, which does the equivalent of
     'taint' to ensure the target label @ldata@ can flow to the
     current label, then throws an exception if @lcurrent@ cannot flow
     back to the target label.

   * When /creating/ or /allocating/ objects, it is permissible for
     them to be higher than the current label, so long as they are
     bellow the current clearance.  In other words, it must be the
     case that @lcurrent ``leq`` ldata && ldata ``leq`` ccurrent@.
     This is ensured by the 'aguard' (allocation guard) function.

The 'taintP', 'wguardP',  and 'aguardP' functions are variants of the
above that take privilege to be more permissive and raise the current
label less. 

-}

-- | General (internal) taint function.  Uses @mylub@ instead of
-- 'lub', so that privileges can optionally be passed in. Throws
-- 'LerrClearance' if raising the current label would exceed the
-- current clearance.
gtaint :: (LabelState l s) =>
          (l -> l -> l)         -- ^ @mylub@ function
       -> l                     -- ^ @l@ - Label to taint with
       -> LIO l s ()
gtaint mylub l = do
  s <- get
  let lnew = l `mylub` (lioL s)
  if lnew `leq` lioC s
     then put s { lioL = lnew }
     else ioTCB $ E.throwIO $ LabeledExceptionTCB (lioL s)
          (toException LerrClearance)

-- |Use @taint l@ in trusted code before observing an object labeled
-- @l@.  This will raise the current label to a value @l'@ such that
-- @l ``leq`` l'@, or throw @'LerrClearance'@ if @l'@ would have to be
-- higher than the current clearance.
taint :: (LabelState l s) => l -> LIO l s ()
taint = gtaint lub

-- |Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that unlike 'setLabelP', @taintP@ will never lower
-- the current label.  It simply uses privileges to avoid raising the
-- label as high as 'taint' would raise it.
taintP :: (Priv l p, LabelState l s)
       => p              -- ^Privileges to invoke
       -> l              -- ^Label to taint to if no privileges
       -> LIO l s ()
taintP p = gtaint (lostar p)

-- |Use @wguard l@ in trusted code before modifying an object labeled
-- @l@.  If @l'@ is the current label, then this function ensures that
-- @l' ``leq`` l@ before doing the same thing as @'taint' l@.  Throws
-- @'LerrHigh'@ if the current label @l'@ is too high.
wguard :: (LabelState l s) => l -> LIO l s ()
wguard l = do l' <- getLabel
              if l' `leq` l
               then taint l
               else throwIO LerrHigh

-- |Like 'wguard', but takes privilege argument to be more permissive.
wguardP :: (Priv l p, LabelState l s) => p -> l -> LIO l s ()
wguardP p l = do l' <- getLabel
                 if leqp p l' l
                  then taintP p l
                  else throwIO LerrHigh

-- |Ensures the label argument is between the current IO label and
-- current IO clearance.  Use this function in code that allocates
-- objects--untrusted code shouldn't be able to create an object
-- labeled @l@ unless @aguard l@ does not throw an exception.
aguard :: (LabelState l s) => l -> LIO l s ()
aguard newl = do c <- getClearance
                 l <- getLabel
                 unless (leq newl c) $ throwIO LerrClearance
                 unless (leq l newl) $ throwIO LerrLow
                 return ()

-- | Like 'aguardP', but takes privilege argument to be more permissive.
aguardP :: (Priv l p, LabelState l s) => p -> l -> LIO l s ()
aguardP p newl = do c <- getClearance
                    l <- getLabel
                    unless (leqp p newl c) $ throwIO LerrClearance
                    unless (leqp p l newl) $ throwIO LerrLow
                    return ()


---------------------------------------------------------------------
-- Labeled Exceptions -----------------------------------------------
---------------------------------------------------------------------

{- $lexception

LIO throws 'LabelFault' exceptions when an information flow violation
is to occur. In general, such exceptions are handled in the outer,
trusted IO code block that executes untrusted LIO code. However, it is
sometimes desirable for untrusted code to throw exceptions. To this
end we provide an implementation of labeled exceptions, as \"normal\"
exceptions are unsafe and can be used to leak information.  We
describe the interface below.

-}

-- | Violation of information flow conditions, or label checks should
-- throw exceptions of type @LabelFault@. The @LerrInval@ constructor
-- takes a string parameter -- it is important that trusted code use
-- this carefully and aovid leaking information through it.
data LabelFault = LerrLow           -- ^ Requested label too low
                | LerrHigh          -- ^ Current label too high
                | LerrClearance     -- ^ Label would exceed clearance
                | LerrPriv          -- ^ Insufficient privileges
                | LerrInval String  -- ^ Invalid request
                deriving Typeable

instance Show LabelFault where
  show LerrLow       = "LerrLow: Requested label is too low"
  show LerrHigh      = "LerrHigh: Requested label is too high"
  show LerrClearance = "LerrClearance: Label would exceed clearance"
  show LerrPriv      = "LerrPriv: Insufficient privileges"
  show (LerrInval s) = "LerrInval: " ++ s

instance Exception LabelFault


{- $throw

   We must re-define the 'throwIO' and 'catch' functions to work in
   the 'LIO' monad.  A complication is that exceptions could
   potentially leak information.  For instance, within a block of code
   wrapped by 'discard', one might examine a secret bit, and throw an
   exception when the bit is 1 but not 0.  Allowing untrusted code to
   catch the exception leaks the bit.

   The solution is to wrap exceptions up with a label.  The exception
   may be caught, but only if the label of the exception can flow to
   the label at the point the catch statement began execution.  For
   compatibility, the 'throwIO', 'catch', and 'onException' functions
   are now methods that work in both the 'IO' or 'LIO' monad.

   If an exception is uncaught in the 'LIO' monad, the 'evalLIO'
   function will unlabel and re-throw the exception, so that it is
   okay to throw exceptions from within the 'LIO' monad and catch them
   within the 'IO' monad.  (Of course, code in the 'IO' monad must be
   careful not to let the 'LIO' code exploit it to exfiltrate
   information.)

   Wherever possible, however, code should use the 'catchP' and
   'onExceptionP' variants that use whatever privilege is available to
   downgrade the exception.  Privileged code that must always run some
   cleanup function can use the 'onExceptionTCB' and 'bracketTCB'
   functions to run the cleanup code on all exceptions.

   /Note/:  Do not use 'throw' (as opposed to 'throwIO') within the
   'LIO' monad.  Because 'throw' can be invoked from pure code, it has
   no notion of current label and so cannot assign an appropriate
   label to the exception.  As a result, the exception will not be
   catchable within the 'LIO' monad and will propagate all the way out
   of the 'evalLIO' function.  Similarly, asynchronous exceptions
   (such as divide by zero or undefined values in lazily evaluated
   expressions) cannot be caught within the 'LIO' monad.

-}

-- | A labeled exception is simply an exception associated with a label.
data LabeledException l = LabeledExceptionTCB l SomeException
  deriving (Typeable)

instance Label l => Show (LabeledException l) where
    show (LabeledExceptionTCB l e) = show e ++ " {" ++ show l ++ "}"

instance (Label l) => Exception (LabeledException l)

instance (LabelState l s) => MonadCatch (LIO l s) where
    mask k = mkLIO $ \s -> E.mask (fun k s)
      where fun :: (LabelState l s)
                => ((forall a. LIO l s a -> LIO l s a) -> LIO l s b)
                -> LIOstate l s
                -> (forall a. IO a -> IO a) -> IO (b, LIOstate l s)
            -- this is a little bit like magic to make types work:
            fun f s = \g -> unLIO (f (\x -> rtioTCB $ g (fst <$> unLIO x s))) s
    -- | It is not possible to catch pure exceptions from within the 'LIO'
    -- monad, but @throwIO@ wraps up an exception with the current label,
    -- so that it can be caught with 'catch' or 'catchP'..
    throwIO e = do
      l <- getLabel
      ioTCB $ E.throwIO $ LabeledExceptionTCB l (toException e)

    -- | Basic function for catching labeled exceptions. (The fact that
    -- they are labeled is hidden from the handler.)
    --
    -- > catch = catchP NoPrivs
    --
    catch = catchP NoPrivs 


-- | Catches an exception, so long as the label at the point where the
-- exception was thrown can flow to the label at which catchP is
-- invoked, modulo the privileges specified.  Note that the handler
-- receives an an extra first argument (before the exception), which
-- is the label when the exception was thrown.
catchP :: (Exception e, Priv l p, LabelState l s)
       => p   -- ^ Privileges with which to downgrade exception
       -> LIO l s a        -- ^ Computation to run
       -> (e -> LIO l s a) -- ^ Exception handler
       -> LIO l s a        -- ^ Result of computation or handler
catchP p io handler = do
  s <- get
  clr <- getClearance
  (a, s') <- ioTCB $ do
    (unLIO io s) `catch` (\e@(LabeledExceptionTCB le se) ->
      case fromException se of
        Nothing -> throwIO e
        Just e' -> if leqp p le clr
                     then unLIO (taintP p le >> handler e') s
                     else throwIO e)
  put s'
  return a

-- | Trusted catch functin.
catchTCB :: (LabelState l s)
         => LIO l s a        -- ^ Computation to run
         -> (LabeledException l -> LIO l s a) -- ^ Exception handler
         -> LIO l s a        -- ^ Result of computation or handler
catchTCB io handler = do
  s <- get
  (a, s') <- ioTCB $ do
    (unLIO io s) `E.catch` (\e -> unLIO (handler e) s)
  put s'
  return a

-- | Version of 'catchP' with arguments swapped.
handleP :: (Exception e, Priv l p, LabelState l s)
        => p   -- ^ Privileges with which to downgrade exception
        -> (e -> LIO l s a) -- ^ Exception handler
        -> LIO l s a        -- ^ Computation to run
        -> LIO l s a        -- ^ Result of computation or handler
handleP p = flip (catchP p)

-- | 'onException' cannot run its handler if the label was raised in
-- the computation that threw the exception.  This variant allows
-- privileges to be supplied, so as to catch exceptions thrown with a
-- raised label.
onExceptionP :: (Priv l p, LabelState l s)
             => p         -- ^ Privileges to downgrade exception
             -> LIO l s a -- ^ The computation to run
             -> LIO l s b -- ^ Handler to run on exception
             -> LIO l s a -- ^ Result if no exception thrown
onExceptionP p io what = catchP p io
   (\e -> what >> throwIO (e :: SomeException))

-- | Like standard 'E.bracket', but with privileges to downgrade
-- exception.
bracketP :: (Priv l p, LabelState l s)
         => p                   -- ^ Priviliges used to downgrade
         -> LIO l s a           -- ^ Computation to run first
         -> (a -> LIO l s c)    -- ^ Computation to run last
         -> (a -> LIO l s b)    -- ^ Computation to run in-between
         -> LIO l s b
bracketP p = genericBracket (onExceptionP p)

-- | Forces its argument to be evaluated to weak head normal form when the
-- resultant LIO action is executed. This is simply a wrapper for 
-- "Control.Exception"'s @evaluate@.
evaluate :: (LabelState l s) => a -> LIO l s a
evaluate = rtioTCB . E.evaluate

-- | For privileged code that needs to catch all exceptions in some
-- cleanup function.  Note that for the 'LIO' monad, these methods do
-- /not/ label the exceptions.  It is assumed that you will use
-- 'rtioTCB' instead of 'ioTCB' for IO within the computation arguments
-- of these methods.
class (MonadCatch m) => OnExceptionTCB m where
    onExceptionTCB :: m a -> m b -> m a
    bracketTCB     :: m a -> (a -> m c) -> (a -> m b) -> m b
    bracketTCB     = genericBracket onExceptionTCB

instance OnExceptionTCB IO where
    onExceptionTCB = E.onException
    bracketTCB     = E.bracket

instance (LabelState l s) => OnExceptionTCB (LIO l s) where
    onExceptionTCB m cleanup = mkLIO $ \s ->
      unLIO m s `catch` (\e -> unLIO cleanup s >> throwIO (e :: SomeException))

instance (LabelState l s) => MonadError IOException (LIO l s) where
    throwError = throwIO
    catchError = catch
