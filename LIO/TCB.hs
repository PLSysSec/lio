{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the core of the Labeled IO library for
-- information flow control in Haskell.  It provides a monad, 'LIO',
-- that is intended to be used as a replacement for the IO monad in
-- untrusted code.  The idea is for untrusted code to provide a
-- computation in the 'LIO' monad, which trusted code can then safely
-- execute through the 'evalLIO' function.  (Though usually a wrapper
-- function is employed depending on the type of labels used by an
-- application.  For example, with "LIO.DCLabel", you would use
-- 'evalDC' to execute an untrusted computation, and with "LIO.HiStar"
-- labels, the function is 'evalHS'.  There are also abbreviations for
-- the 'LIO' monad type of a particular label--for instance 'DC' or
-- 'HS'.)
--
-- A data structure 'Labeled' (labeled value) protects access to pure
-- values.  Without the appropriate privileges, one cannot produce a
-- pure value that depends on a secret 'Labeled', or conversely produce a
-- high-integrity 'Labeled' based on pure data.  The function 'toLabeled'
-- allows one to seal off the results of an LIO computation inside an
-- 'Labeled' without tainting the current flow of execution.  'unlabel'
-- conversely allows one to use the value stored within a 'Labeled'.
-- 
-- Any code that imports this module is part of the
-- /Trusted Computing Base/ (TCB) of the system.  Hence, untrusted
-- code must be prevented from importing this module.  The exported
-- symbols ending ...@TCB@ can be used to violate label protections
-- even from within pure code or the LIO Monad.  A safe subset of
-- these symbols is exported by the "LIO.Base" module, which is how
-- untrusted code should access the core label functionality.
-- ("LIO.Base" is also re-exported by "LIO.LIO", the main gateway to
-- this library.)
--
module LIO.TCB (
               -- * Basic label functions
               -- $labels
               POrdering(..), POrd(..), o2po, Label(..)
               , Priv(..), NoPrivs(..)
               -- * Labeled IO Monad (LIO)
               -- $LIO
               , LIO
               , getLabel, setLabelP
               , getClearance, lowerClr, lowerClrP, withClearance
               -- ** LIO guards
               -- $guards
               , taint, taintP
               , wguard, wguardP, aguard, aguardP
               -- * Labeled values
               , Labeled
               --
               , label, labelP
               , unlabel, unlabelP
               , toLabeled, toLabeledP, discard
               , labelOf
               , taintLabeled
               -- * Exceptions
               -- ** Exception type thrown by LIO library
               , LabelFault(..)
               -- ** Throwing and catching labeled exceptions
               -- $lexception
               , MonadCatch(..), catchP, handleP, onExceptionP, bracketP
               -- * Executing computations
               , evaluate
               , evalLIO
               -- Start TCB exports
               -- * Privileged operations
               , LIOstate(..)
               , runLIO
               , ShowTCB(..)
               , ReadTCB(..)
               , labelTCB
               , PrivTCB, MintTCB(..)
               , unlabelTCB
               , setLabelTCB, lowerClrTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               -- ** Misc symbols useful for privileged code
               , newstate, LIOstate, runLIO
               -- End TCB exports
               ) where
import Debug.Trace
import Prelude hiding (catch)
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State.Lazy hiding (put, get)
import Control.Exception hiding (catch, handle, throw, throwIO,
                                 onException, block, unblock, evaluate)
import qualified Control.Exception as E
import Data.Monoid
import Data.Typeable
import Text.Read (minPrec)

import LIO.MonadCatch

--
-- We need a partial order and a Label
--

{- $labels

Labels are a way of describing who can observe and modify data.  There
is a partial order, generally pronounced \"can flow to\" on labels.
In Haskell we write this partial order ``leq`` (in the literature it
is usually written as a square less than or equal sign--@\\sqsubseteq@
in TeX).

The idea is that data labeled @l1@ should affect data labeled @l2@ only if
@l1@ ``leq`` @l2@, (i.e., @l1@ /can flow to/ @l2@).  The 'LIO' monad keeps
track of the current label of the executing code (accessible via the
'getLabel' function).  Code may attempt to perform various IO or
memory operations on labeled data.  Touching data may change the
current label or throw an exception if an operation would violate
can-flow-to restrictions.

If the current label is @lcurrent@, then it is only permissible to
read data labeled @lr@ if @lr ``leq`` lcurrent@.  This is sometimes
termed \"no read up\" in the literature; however, because the partial
order allows for incomparable labels (i.e., two labels @l1@ and @l2@
such that @not (l1 ``leq`` l2) && not (l2 ``leq`` l1)@), a more
appropriate phrasing would be \"read only what can flow to your
label\".  Note that, rather than throw an exception, reading data will
often just increase the current label to ensure that @lr ``leq``
lcurrent@.  The LIO monad keeps a second label, called the /clearance/
(see 'getClearance'), that represents the highest value the
current thread can raise its label to.

Conversely, it is only permissible to modify data labeled @lw@ when
@lcurrent ``leq`` lw@, a property often cited as \"no write down\",
but more accurately characterized as \"write only what you can flow
to\".  In practice, there are very few IO abstractions in which it is
possible to do a pure write that doesn't also involve observing some
state.  For instance, writing to a file handle and not getting an
exception tells you that the handle is not closed.  Thus, in practice,
the requirement for modifying data labeled @lw@ is almost always that
@lcurrent == lw@.

Note that higher labels are neither more nor less privileged than
lower ones.  Simply, the higher one's label is, the more things one
can read.  Conversely, the lower one's label, the more things one can
write.  But, because labels are a partial and not a total order, 
some data may be completely inaccessible to a particular omputatoin; for
instance, if the current label is @lcurrent@, the current clearance is
@ccurrent@, and some data is labeled @ld@, such that @not (lcurrent
``leq`` ld || ld ``leq`` ccurrent)@, then the current thread can
neither read nor write the data, at least without invoking some
privilege.

Privilege comes from a separate class called 'Priv', representing the
ability to bypass the protection of certain labels.  Essentially,
privilege allows you to behave as if @l1 ``leq`` l2@ even when that is
not the case.  The process of making data labeled @l1@ affect data
labeled @l2@ when @not (l1 ``leq`` l2)@ is called /downgrading/.

The basic method of the 'Priv' object is 'leqp', which performs the
more permissive can-flow-to check in the presence of particular
privileges.  Many 'LIO' operations have variants ending @...P@ that
take a privilege argument to act in a more permissive way.  All 'Priv'
types are monoids, and so can be combined with 'mappend'.

How to create 'Priv' objects is specific to the particular label type
in use.  The method used is 'mintTCB', but the arguments depend on the
particular label type.  (Obviously, the symbol 'mintTCB' must not be
available to untrusted code.)

-}

data POrdering = PEQ            -- ^Equal
               | PLT            -- ^Less than
               | PGT            -- ^Greater than
               | PNE
                 -- ^Incomparable (neither less than nor greater than)
                 deriving (Eq, Ord, Show)

instance Monoid POrdering where
    mempty          = PEQ
    mappend PLT PGT = PNE
    mappend PGT PLT = PNE
    mappend x y     = max x y

class (Eq a) => POrd a where
    pcompare :: a -> a -> POrdering
    leq :: a -> a -> Bool

    pcompare a b | a == b = PEQ
                 | a `leq` b = PLT
                 | b `leq` a = PGT
                 | otherwise = PNE
    leq a b = case pcompare a b of
                PEQ -> True
                PLT -> True
                _   -> False

o2po :: Ordering -> POrdering
o2po EQ = PEQ; o2po LT = PLT; o2po GT = PGT

class (POrd a, Show a, Read a, Typeable a) => Label a where
    -- | bottom
    lbot :: a
    -- | top
    ltop :: a
    -- | least upper bound (join) of two labels
    lub :: a -> a -> a
    -- | greatest lower bound (meet) of two labels
    glb :: a -> a -> a


--
-- Labeled value - Labeled
-- Downgrading privileges - Priv
--

-- | @Labeled@ is a type representing labeled data.  
data (Label l) => Labeled l t = LabeledTCB l t

-- | It would be a security issue to make certain objects a member of
-- the 'Show' class, but nonetheless it is useful to be able to
-- examine such objects from within the debugger.  The 'showTCB'
-- method can be used to examine such objects.
class ShowTCB a where
    showTCB :: a -> String

instance (Label l, Show a) => ShowTCB (Labeled l a) where
    showTCB (LabeledTCB l t) = shows t $ " {" ++ shows l "}"

class MintTCB t i where
    -- |A function that mints new objects (such as instances of
    -- 'Priv') in a way that only privileged code should be allowed to
    -- do.  Because the MintTCB method is only available to
    -- priviledged code, other modules imported by unpriviledged code
    -- can define instances of mintTCB.
    mintTCB :: i -> t

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

-- | @PrivTCB@ is a method-less class whose only purpose is to be
-- unavailable to unprivileged code.  Since @(PrivTCB t) =>@ is in the
-- context of class 'Priv' and unprivileged code cannot create new
-- instances of the @PrivTCB@ class, this ensures unprivileged code
-- cannot create new instances of the 'Priv' class either, even though
-- the symbol 'Priv' is exported by "LIO.Base" and visible to
-- untrusted code.
class PrivTCB t where
class (Label l, Monoid p, PrivTCB p) => Priv l p where
    -- |@leqp p l1 l2@ means that privileges @p@ are sufficient to
    -- downgrade data from @l1@ to @l2@.  Note that @'leq' l1 l2@
    -- implies @'leq' p l1 l2@ for all @p@, but for some labels and
    -- privileges, @leqp@ will hold even where @'leq'@ does not.
    leqp :: p -> l -> l -> Bool
    leqp p a b = lostar p a b `leq` b

    -- | Roughly speaking, the function
    -- 
    -- > result = lostar p label goal
    -- 
    -- computes how close one can come to downgrading data labeled
    -- @label@ to @goal@ given privileges @p@.  When @p == 'NoPrivs'@,
    -- @result == 'lub' label goal@.  If @p@ contains all possible
    -- privileges, then @result == goal@.
    --
    -- More specifically, @result@ is the greatest lower bound of the
    -- set of all labels @r@ satisfying:
    --
    --   1. @'leq' goal r@, and
    --
    --   2. @'leqp' p label r@
    --
    -- Operationally, @lostar@ captures the minimum change required to
    -- the current label when viewing data labeled @label@.  A common
    -- pattern is to use the result of 'getLabel' as @goal@ (i.e.,
    -- the goal is to use privileges @p@ to avoid changing the label
    -- at all), and then compute @result@ based on the @label@ of data
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

-- Trusted constructor that creates labeled values.
labelTCB :: Label l => l -> a -> Labeled l a
labelTCB l a = LabeledTCB l a

-- | Raises the label of a 'Labeled' to the 'lub' of it's current label
-- and the value supplied.  The label supplied must be less than the
-- current clarance, though the resulting label may not be if the
-- 'Labeled' is already above the current thread's clearance.
taintLabeled :: (Label l) => l -> Labeled l a -> LIO l s (Labeled l a)
taintLabeled l (LabeledTCB la a) = do
  aguard l
  return $ LabeledTCB (lub l la) a

-- | Extracts the value from an 'Labeled', discarding the label and any
-- protection.
unlabelTCB :: Label l => Labeled l a -> a
unlabelTCB (LabeledTCB _ a) = a

-- | Returns label of a 'Label' type.
labelOf :: Label l => Labeled l a -> l
labelOf (LabeledTCB l _) = l


--
-- Labeled IO
--

-- $LIO
-- 
-- The 'LIO' monad is a wrapper around 'IO' that keeps track of the
-- current label and clearance.  It is possible to raise one's label
-- or lower one's clearance without privilege, but moving in the other
-- direction requires appropriate privilege.
--
-- 'LIO' is parameterized by two types.  The first is the particular
-- label type.  The second type is state specific to the label type.
-- Trusted label implementation code can use 'getTCB' and 'putTCB' to
-- get and set the label state.

data (Label l) => LIOstate l s =
    LIOstate { labelState :: s
             , lioL :: l -- current label
             , lioC :: l -- current clearance
             }

newtype (Label l) => LIO l s a = LIO (StateT (LIOstate l s) IO a)
    deriving (Functor, Monad, MonadFix)

get :: (Label l) => LIO l s (LIOstate l s)
get = mkLIO $ \s -> return (s, s)

put :: (Label l) => LIOstate l s -> LIO l s ()
put s = mkLIO $ \_ -> return (() , s)

-- | Function to construct an 'Labeled' from a label and pure value.  If
-- the current label is @lcurrent@ and the current clearance is
-- @ccurrent@, then the label @l@ specified must satisfy
-- @lcurrent ``leq`` l && l ``leq`` ccurrent@.
label :: (Label l) => l -> a -> LIO l s (Labeled l a)
label l a = get >>= doit
    where doit s | not $ l `leq` lioC s = throwIO LerrClearance
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = return $ LabeledTCB l a

-- | Constructs an 'Labeled' using privilege to allow the `Labeled`'s label
-- to be below the current label.  If the current label is @lcurrent@
-- and the current clearance is @ccurrent@, then the privilege @p@ and
-- label @l@ specified must satisfy
-- @(leqp p lcurrent l) && l ``leq`` ccurrent@.
-- Note that privilege is not used to bypass the clearance.  You must
-- use 'lowerClrP' to raise the clearance first if you wish to
-- create an 'Labeled' at a higher label than the current clearance.
labelP :: (Priv l p) => p -> l -> a -> LIO l s (Labeled l a)
labelP p l a = get >>= doit
    where doit s | not $ l `leq` lioC s    = throwIO LerrClearance
                 | not $ leqp p (lioL s) l = throwIO LerrLow
                 | otherwise               = return $ LabeledTCB l a

-- | Returns the current value of the thread's label.
getLabel :: (Label l) => LIO l s l
getLabel = get >>= return . lioL

-- | Returns the current value of the thread's clearance.
getClearance :: (Label l) => LIO l s l
getClearance = get >>= return . lioC

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
-- 'lub', so that privileges can optionally be passed in.  Throws
-- 'LerrClearance' if raising the current label would exceed the
-- current clearance.
gtaint :: (Label l) =>
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
taint :: (Label l) => l -> LIO l s ()
taint = gtaint lub

-- |Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that unlike 'setLabelP', @taintP@ will never lower
-- the current label.  It simply uses privileges to avoid raising the
-- label as high as 'taint' would raise it.
taintP :: (Priv l p) =>
              p              -- ^Privileges to invoke
           -> l              -- ^Label to taint to if no privileges
           -> LIO l s ()
taintP p = gtaint (lostar p)

-- |Use @wguard l@ in trusted code before modifying an object labeled
-- @l@.  If @l'@ is the current label, then this function ensures that
-- @l' ``leq`` l@ before doing the same thing as @'taint' l@.  Throws
-- @'LerrHigh'@ if the current label @l'@ is too high.
wguard :: (Label l) => l -> LIO l s ()
wguard l = do l' <- getLabel
              if l' `leq` l
               then taint l
               else throwIO LerrHigh

-- |Like 'wguard', but takes privilege argument to be more permissive.
wguardP :: (Priv l p) => p -> l -> LIO l s ()
wguardP p l = do l' <- getLabel
                 if leqp p l' l
                  then taintP p l
                  else throwIO LerrHigh

-- |Ensures the label argument is between the current IO label and
-- current IO clearance.  Use this function in code that allocates
-- objects--untrusted code shouldn't be able to create an object
-- labeled @l@ unless @aguard l@ does not throw an exception.
aguard :: (Label l) => l -> LIO l s ()
aguard newl = do c <- getClearance
                 l <- getLabel
                 unless (leq newl c) $ throwIO LerrClearance
                 unless (leq l newl) $ throwIO LerrLow
                 return ()

-- | Like 'aguardP', but takes privilege argument to be more permissive.
aguardP :: (Priv l p) => p -> l -> LIO l s ()
aguardP p newl = do c <- getClearance
                    l <- getLabel
                    unless (leqp p newl c) $ throwIO LerrClearance
                    unless (leqp p l newl) $ throwIO LerrLow
                    return ()


-- | If the current label is @oldLabel@ and the current clearance is
-- @clearance@, this function allows code to raise the label to any
-- value @newLabel@ such that
-- @oldLabel ``leq`` newLabel && newLabel ``leq`` clearance@.
-- Note that there is no @setLabel@ variant without the @...P@ because
-- the 'taint' function provides essentially the same functionality
-- that @setLabel@ would.
setLabelP :: (Priv l p) => p -> l -> LIO l s ()
setLabelP p l = do s <- get
                   if leqp p (lioL s) l
                     then put s { lioL = l }
                     else throwIO LerrPriv

-- | Set the current label to anything, with no security check.
setLabelTCB :: (Label l) => l -> LIO l s ()
setLabelTCB l = do s <- get
                   if l `leq` lioC s
                      then put s { lioL = l }
                      else throwIO LerrClearance

-- |Reduce the current clearance.  One cannot raise the current label
-- or create object with labels higher than the current clearance.
lowerClr :: (Label l) => l -> LIO l s ()
lowerClr l = get >>= doit
    where doit s | not $ l `leq` lioC s = throwIO LerrClearance
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = put s { lioC = l }

-- |Raise the current clearance (undoing the effects of 'lowerClr').
-- This requires privileges.
lowerClrP :: (Priv l p) => p -> l -> LIO l s ()
lowerClrP p l = get >>= doit
    where doit s | not $ leqp p l $ lioC s = throwIO LerrPriv
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = put s { lioC = l }

-- | Set the current clearance to anything, with no security check.
lowerClrTCB :: (Label l) => l -> LIO l s ()
lowerClrTCB l = get >>= doit
    where doit s | not $ lioL s `leq` l = throwIO LerrInval
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
withClearance :: (Label l) => l -> LIO l s a -> LIO l s a
withClearance l m = do
  s <- get
  put s { lioC = l `glb` lioC s }
  a <- m
  put s { lioC = lioC s }
  return a

-- | Within the 'LIO' monad, this function takes an 'Labeled' and returns
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
unlabel :: (Label l) => Labeled l a -> LIO l s a
unlabel (LabeledTCB la a) = do
  taint la
  return a

-- | Extracts the value of an 'Labeled' just like 'unlabel', but takes a
-- privilege argument to minimize the amount the current label must be
-- raised.  Will still throw 'LerrClearance' under the same
-- circumstances as 'unlabel'.
unlabelP :: (Priv l p) => p -> Labeled l a -> LIO l s a
unlabelP p (LabeledTCB la a) = do
  taintP p la
  return a

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
toLabeled :: (Label l) => l -> LIO l s a -> LIO l s (Labeled l a)
toLabeled = toLabeledP NoPrivs

toLabeledP :: (Priv l p) => p -> l -> LIO l s a -> LIO l s (Labeled l a)
toLabeledP p l m = do
  aguard l
  LIOstate { lioL = l, lioC = c } <- get
  a <- m
  s <- get
  put s { lioL = l, lioC = c }
  unless (leqp p (lioL s) l) $ throwIO LerrLow -- l is too low
  return $ LabeledTCB l a

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
discard :: (Label l) =>  l -> LIO l s a -> LIO l s ()
discard l m = toLabeled l m >> return ()

-- | Returns label-specific state of the 'LIO' monad.  This is the
-- data specified as the second argument of 'evalLIO', whose type is
-- @s@ in the monad @LIO l s@.
getTCB :: (Label l) => LIO l s s
getTCB = get >>= return . labelState

-- | Sets the label-specific state of the 'LIO' monad.  See 'getTCB'.
putTCB :: (Label l) => s -> LIO l s ()
putTCB ls = get >>= put . update
    where update s = s { labelState = ls }

-- | Generate a fresh state to pass 'runLIO' when invoking it for the
-- first time.
newstate :: (Label l) => s -> LIOstate l s
newstate s = LIOstate { labelState = s , lioL = lbot , lioC = ltop }

mkLIO :: (Label l) => (LIOstate l s -> IO (a, LIOstate l s)) -> LIO l s a
mkLIO = LIO . StateT

unLIO :: (Label l) => LIO l s a -> LIOstate l s
                       -> IO (a, LIOstate l s)
unLIO (LIO (StateT f)) = f

-- | Execute an LIO action.
runLIO :: forall l s a. (Label l) => LIO l s a -> LIOstate l s
       -> IO (a, LIOstate l s)
runLIO m s = unLIO m s `E.catch` (E.throwIO . delabel)
    where delabel :: LabeledExceptionTCB l -> SomeException
          delabel (LabeledExceptionTCB _ e) = e
           -- trace ("unlabeling " ++ show e ++ " {" ++ show l ++ "}") e

-- | Produces an 'IO' computation that will execute a particular 'LIO'
-- computation.  Because untrusted code cannot execute 'IO'
-- computations, this function should only be useful within trusted
-- code.  No harm is done from exposing the @evalLIO@ symbol to
-- untrusted code.  (In general, untrusted code is free to produce
-- 'IO' computations--it just can't execute them without access to
-- 'ioTCB'.)
evalLIO :: (Label l) =>
           LIO l s a -- ^ The LIO computation to execute
        -> s         -- ^ Initial value of label-specific state
        -> IO (a, l) -- ^ IO computation that will execute first argument
evalLIO m s = do (a, ls) <- runLIO m (newstate s)
                 return (a, lioL ls)

-- | Lifts an 'IO' computation into the 'LIO' monad.  Note that
-- exceptions thrown within the 'IO' computation cannot directly be
-- caught within the 'LIO' computation.  Thus, if you are not inside a
-- 'rethrowTCB' block, you will generally want to use 'rtioTCB'
-- instead of 'ioTCB'.
ioTCB :: (Label l) => IO a -> LIO l s a
ioTCB a = mkLIO $ \s -> do r <- a; return (r, s)

-- | Lifts an 'IO' computation into the 'LIO' monad.  If the 'IO'
-- computation throws an exception, it labels the exception with the
-- current label so that the exception can be caught with 'catch' or
-- 'catchP'.  This function's name stands for \"re-throw io\", because
-- the functionality is a combination of 'rethrowTCB' and 'ioTCB'.
-- Effectively
--
-- @
--   rtioTCB = 'rethrowTCB' . 'ioTCB'
-- @
rtioTCB :: (Label l) => IO a -> LIO l s a
rtioTCB a = rethrowTCB $ mkLIO $ \s -> do r <- a; return (r, s)

--
-- Exceptions
--

data LabelFault
    = LerrLow                   -- ^ Requested label too low
    | LerrHigh                  -- ^ Current label too high
    | LerrClearance             -- ^ Label would exceed clearance
    | LerrPriv                  -- ^ Insufficient privileges
    | LerrInval                 -- ^ Invalid request
      deriving (Show, Typeable)

instance Exception LabelFault

-- | Map a function over the underlying IO computation within an LIO.
-- Obviously this symbol should not be exported, as it is privileged.
iomaps :: (Label l) =>
          (LIOstate l s -> IO (a, LIOstate l s) -> IO (a, LIOstate l s))
       -> LIO l s a -> LIO l s a
iomaps f c = mkLIO $ \s -> (f s $ unLIO c s)

-- | Like 'iomaps' if you don't need the state.
iomap :: (Label l) =>
         (IO (a, LIOstate l s) -> IO (a, LIOstate l s))
      -> LIO l s a -> LIO l s a
iomap f = iomaps $ \_ -> f

-- |Privileged code that does IO operations may cause exceptions that
-- should be caught by untrusted code in the 'LIO' monad.  Such
-- operations should be wrapped by @rethrowTCB@ (or 'rtioTCB', which
-- uses @rethrowTCB@) to ensure the exception is labeled.  Note that
-- it is very important that the computation executed inside
-- @rethrowTCB@ not in any way change the label, as otherwise
-- @rethrowTCB@ would put the wrong label on the exception.
rethrowTCB :: (Label l) => LIO l s a -> LIO l s a
rethrowTCB = iomaps $ \s -> handle (E.throwIO . (LabeledExceptionTCB $ lioL s))
 -- mapException (LabeledExceptionTCB $ lioL s) c

data LabeledExceptionTCB l =
    LabeledExceptionTCB l SomeException deriving Typeable

{- $lexception

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

   Note:  Do not use 'throw' (as opposed to 'throwIO') within the
   'LIO' monad.  Because 'throw' can be invoked from pure code, it has
   no notion of current label and so cannot assign an appropriate
   label to the exception.  As a result, the exception will not be
   catchable within the 'LIO' monad and will propagate all the way out
   of the 'evalLIO' function.  Similarly, asynchronous exceptions
   (such as divide by zero or undefined values in lazily evaluated
   expressions) cannot be caught within the 'LIO' monad.

-}

instance Label l => Show (LabeledExceptionTCB l) where
    showsPrec _ (LabeledExceptionTCB l e) rest =
        shows e $ (" {" ++) $ shows l $ "}" ++ rest

instance (Label l) => Exception (LabeledExceptionTCB l)

instance (Label l) => MonadCatch (LIO l s) where
    block   = iomap E.block
    unblock = iomap E.unblock
    -- |It is not possible to catch pure exceptions from within the 'LIO'
    -- monad, but @throwIO@ wraps up an exception with the current label,
    -- so that it can be caught with 'catch' or 'catchP'..
    throwIO e = mkLIO $ \s -> E.throwIO $
                LabeledExceptionTCB (lioL s) (toException e)
    -- | Basic function for catching labeled exceptions.  (The fact that
    -- they are labeled is hidden from the handler.)
    --
    -- > catch m c = catchP m NoPrivs (\_ -> c)
    --
    catch m c = iomaps (\s m' -> m' `E.catch` doit s) m
        where
          doit s e@(LabeledExceptionTCB l se) =
              case fromException se of
                Just e' | l `leq` lioC s ->
                            unLIO (c e') s { lioL = (lioL s) `lub` l }
                _                        -> E.throwIO e



-- | Catches an exception, so long as the label at the point where the
-- exception was thrown can flow to the label at which catchP is
-- invoked, modulo the privileges specified.  Note that the handler
-- receives an an extra first argument (before the exception), which
-- is the label when the exception was thrown.
catchP :: (Label l, Exception e, Priv l p) =>
                p   -- ^ Privileges with which to downgrade exception
             -> LIO l s a             -- ^ Computation to run
             -> (l -> e -> LIO l s a) -- ^ Exception handler
             -> LIO l s a             -- ^ Result of computation or handler
catchP p m c = iomaps (\s m' -> m' `E.catch` doit s) m
    where
      doit s e@(LabeledExceptionTCB l se) =
          case fromException se of
            Just e' -> let lnew = lostar p l (lioL s)
                       in if leq lnew $ lioC s
                          then unLIO (c l e') s { lioL = lnew }
                          else E.throw e
            _       -> E.throw e

-- | Version of 'catchP' with arguments swapped.
handleP :: (Label l, Exception e, Priv l p) =>
                 p   -- ^ Privileges with which to downgrade exception
              -> (l -> e -> LIO l s a) -- ^ Exception handler
              -> LIO l s a             -- ^ Computation to run
              -> LIO l s a             -- ^ Result of computation or handler
handleP p = flip (catchP p)

-- | 'onException' cannot run its handler if the label was raised in
-- the computation that threw the exception.  This variant allows
-- privileges to be supplied, so as to catch exceptions thrown with a
-- raised label.
onExceptionP :: (Priv l p) =>
                          p         -- ^ Privileges to downgrade exception
                       -> LIO l s a -- ^ The computation to run
                       -> LIO l s b -- ^ Handler to run on exception
                       -> LIO l s a -- ^ Result if no exception thrown
onExceptionP p io what = catchP p io
                         (\_ e -> what >> throwIO (e :: SomeException))

-- | Like standard 'E.bracket', but with privileges to downgrade
-- exception.
bracketP :: (Priv l p) =>
            p
         -> LIO l s a
         -> (a -> LIO l s c)
         -> (a -> LIO l s b)
         -> LIO l s b
bracketP p = genericBracket (onExceptionP p)

-- | For privileged code that needs to catch all exceptions in some
-- cleanup function.  Note that for the 'LIO' monad, these methods do
-- /not/ call 'rethrowTCB' to label the exceptions.  It is assumed
-- that you will use 'rtioTCB' instead of 'ioTCB' for IO within the
-- computation arguments of these methods.
class (MonadCatch m) => OnExceptionTCB m where
    onExceptionTCB :: m a -> m b -> m a
    bracketTCB :: m a -> (a -> m c) -> (a -> m b) -> m b
    bracketTCB = genericBracket onExceptionTCB
instance OnExceptionTCB IO where
    onExceptionTCB = E.onException
    bracketTCB     = E.bracket
instance (Label l) => OnExceptionTCB (LIO l s) where
    onExceptionTCB m cleanup = 
        mkLIO $ \s -> unLIO m s `E.catch` \e ->
        unLIO cleanup s >> E.throwIO (e :: SomeException)

instance (Label l) => MonadError IOException (LIO l s) where
    throwError = throwIO
    catchError = catch

-- | Evaluate in LIO.
evaluate :: (Label l) => a -> LIO l s a
evaluate = rtioTCB . E.evaluate
