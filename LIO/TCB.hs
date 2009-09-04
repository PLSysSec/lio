{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the core (Trusted Computing Base) of the
-- Labeled IO library for information flow control in Haskell.  It
-- provides a monad, 'LIO', that is intended to be used as a
-- replacement for the IO monad in untrusted code.  The idea is for
-- untrusted to provide a computation in the 'LIO' monad, which
-- trusted code can then safely execute through the 'evalLIO'
-- function.  (Though usually a wrapper function is employed depending
-- on the type of labels used by an application.  For example, with
-- "LIO.DCLabels", you would use 'evalDC' to execute an untrusted
-- computation, and with "LIO.HiStar" labels, the function is
-- 'evalHS'.  There are also abbreviations for the 'LIO' monad type of
-- a particular label--for instance 'DC' or 'HS'.)
--
-- It provides a data structure 'Lref' (labeled reference), which
-- protects access to pure values.  Without the appropriate
-- privileges, one cannot produce a pure value that depends on a
-- secret 'Lref', or conversely produce a high-integrity 'Lref' based
-- on pure data.
--
-- Untrusted code must be prevented from importing this module.  The
-- exported symbols ending ...@TCB@ can be used to violate label
-- protections even from within pure code or the LIO Monad.  A safe
-- subset of these symbols is exported by the "LIO.Base" module, which
-- is how untrusted code should access the core label functionality.
module LIO.TCB (
               -- * Basic label functions
               -- $labels
               POrdering(..), POrd(..), o2po, Label(..)
               , Priv(..), NoPrivs(..)
               -- * Labeled IO Monad (LIO)

               -- | The 'LIO' monad is a wrapper around 'IO' that
               -- keeps track of the current label and clearance.  It
               -- is possible to raise one's label or lower one's
               -- clearance without privilege, but to move in the
               -- other direction requires appropriate privilege.
               , LIO
               , currentLabel, currentClearance
               , setLabelP , setClearance, setClearanceP
               -- ** LIO guards
               -- $guards
               , taint, taintP, wguard, wguardP, aguard
               -- * References to labeled pure data (LRefs)
               , Lref
               , lref, unlrefP
               , taintR, guardR, setLabelRP
               , openR, closeR, discardR
               -- * Exceptions
               -- ** Exception type thrown by LIO library
               , LabelFault(..)
               -- ** Throwing and catching labeled exceptions
               -- $lexception
               , MonadCatch(..), catchP, onExceptionP
               , MonadBlock(..)
               -- * Executing computations
               , evalLIO
               -- Start TCB exports
               -- * Privileged operations
               , ShowTCB(..)
               , lrefTCB
               , PrivTCB, MintTCB(..)
               , unlrefTCB, setLabelTCB, setClearanceTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               -- End TCB exports
               ) where

import Prelude hiding (catch)
import Control.Monad.State.Lazy hiding (put, get)
import Control.Exception hiding (catch, throw, throwIO,
                                 onException, block, unblock)
import qualified Control.Exception as E
import Data.Monoid
import Data.Typeable

{- Things to worry about:

   - Because code can look at an Lref, it is possible to leak data in
     Lclose.  For example, look at a secret bit, and if it is 1, then
     taint yourself in a whole bunch more categories.

   - Similar to above, but even without Lclose, just taint yourself
     within the Lref.

   - unsafe... functions must be blocked

   - inlinePerformIO must be blocked

   - Allowing untrusted code to define instances Typeable with bogus
     typeOf functions could lead to unsafe casts.

   - Some way of showing an Lref or even just the label, by putting it
     into an exception.

 -}

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
'currentLabel' function).  Code may attempt to perform various IO or
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
(see 'currentClearance'), that represents the highest value the
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
some data may be completely inaccessible to a particular thread; for
instance, if the current label is @lcurrent@, the current clearance is
@ccurrent@, and some data is labeled @ld@, such that @not (lcurrent
``leq`` ld || ld ``leq`` ccurrent)@, then the current thread can
neither read nor write the data, at least without invoking some
privilege.

Privilege comes from a separate class called 'Priv', representing the
ability to bypass the protection of certain labels.  Essentially,
privilege allows you to behave as if @l1 ``leq`` l2@ even when that is
not the case.  The basic operation on the 'Priv' object is 'leqp',
which performs the more permissive can-flow-to check given particular
privileges.  Many 'LIO' operations have variants ending @...P@ that
take a privilege argument to act in a more permissive way.  All 'Priv'
types are monoids, and so can be combined with 'mappend'.

How to generate privileges is specific to the particular label type in
use.  The method used is 'mintTCB', but the arguments depend on the
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
                PEQ       -> True
                PLT       -> True
                otherwise -> False

o2po :: Ordering -> POrdering
o2po EQ = PEQ; o2po LT = PLT; o2po GT = PGT
-- instance (Ord a) => POrd a where pcompare = o2po . compare

class (POrd a, Show a, Read a, Typeable a) => Label a where
    -- | label for pure values
    lpure :: a
    -- | default /clearance/ (highest value one can raise one's label to)
    lclear :: a
    -- | least upper bound (join) of two labels
    lub :: a -> a -> a
    -- | greatest lower bound (meet) of two labels
    glb :: a -> a -> a


--
-- Labeled value - Lref
-- Downgrading privileges - Priv
--

data (Label l) => Lref l t = Lref l t

-- | It would be a security issue to make certain objects a member of
-- the show class, but nonetheless it is useful to be able to examine
-- such objects from within the debugger.  The 'showTCB' method can be
-- used to examine such objects.
class ShowTCB a where
    showTCB :: a -> String

instance (Label l, Show a) => ShowTCB (Lref l a) where
    showTCB (Lref l t) = shows t $ " {" ++ shows l "}"

instance Label l => Functor (Lref l) where
    fmap f (Lref l t) = Lref (l `lub` lpure) (f t)

instance Label l => Monad (Lref l) where
    return x = Lref lpure x
    (Lref l x) >>= f = let Lref l' y = f x in Lref (lub l l') y

instance Label l => MonadFix (Lref l) where
    mfix f = fix g
        where g ~(Lref _ a) = f a

-- |PrivTCB is a method-less class whose only purpose is to be
-- unavailable to unprivileged code.  Since (PrivTCB t) => is in the
-- context of class 'Priv', this ensures that, since unprivileged code
-- cannot create new instances of the PrivTCB class, it won't be able
-- to create new instances of the 'Priv' class either, even though the
-- symbol 'Priv' is exported by "LIO.Base".
class PrivTCB t where
class (PrivTCB t) => MintTCB t i where
    -- |The function that mints new privileges.
    mintTCB :: i -> t
class (Label l, Monoid p, PrivTCB p) => Priv l p where
    -- |@leqp p l1 l2@ means that privileges @p@ are sufficient to
    -- downgrade data from @l1@ to @l2@.  Note that @'leq' l1 l2@
    -- implies @'leq' p l1 l2@ for all @p@, but for some labels and
    -- privileges, values @leqp@ will hold even if @'leq'@ does not.
    leqp :: p -> l -> l -> Bool
    leqp p a b = lostar p a b `leq` b

    -- |@lostar p source minimum@ returns the lowest label to which
    -- one can downgrade data labeled @source@ given privileges @p@,
    -- least-upper-bounded with @minimum@.  (Without @minimum@, the
    -- representation of the lowest label might have size exponential
    -- in the size of @p@ for some label formats.)  More concretely,
    -- the result returned is the lowest @lres@ such that:  @'leqp' p
    -- source lres && 'leq' minimum lres@
    --
    -- This is useful if your label is originally @l1@, and you touch
    -- some stuff labeled @l2@ but want to minimize the amount of
    -- taint @l2@ causes you.  After raising your label to @l2@, you
    -- can use the privileges in @p@ to lower your label to @lostar p
    -- l2 l1@.
    lostar :: p -> l -> l -> l

-- |A generic 'Priv' instance that works for all 'Label's and confers
-- no downgrading privileges.
data NoPrivs = NoPrivs
instance PrivTCB NoPrivs
instance Monoid NoPrivs where
    mempty      = NoPrivs
    mappend _ _ = NoPrivs
instance (Label l) => Priv l NoPrivs where
    leqp _ a b     = leq a b
    lostar _ l min = lub l min

lrefTCB     :: Label l => l -> a -> Lref l a
lrefTCB l a = Lref l a

--
-- Since this function has a covert channel, don't export for now.
-- Really what we need is a way to check that an LRef has some
-- particular integrity category (or is below some label that we are
-- allowed to read).
--
labelOfR            :: Label l => Lref l a -> l
labelOfR (Lref l a) = l

taintR               :: (Label l) => l -> Lref l a -> Lref l a
taintR l' (Lref l a) = Lref (lub l l') a

guardR :: (Label l) => l -> Lref l a -> LIO l s ()
guardR l' (Lref l a) = do
  cur <- currentLabel
  unless (l `leq` l' && l `leq` cur) $ throwIO LerrHigh

setLabelRP                   :: Priv l p => p -> l -> Lref l a -> Lref l a
setLabelRP p newl (Lref l a) = if leqp p l newl then Lref newl a else undefined

unlrefP              :: Priv l p => p -> Lref l a -> a
unlrefP p (Lref l a) = if leqp p l lpure then a else undefined

unlrefTCB            :: Label l => Lref l a -> a
unlrefTCB (Lref l a) = a


--
-- Labeled IO
--

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

lref     :: (Label l) => l -> a -> LIO l s (Lref l a)
lref l a = get >>= doit
    where doit s | not $ l `leq` lioC s = throwIO LerrClearance
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = return $ Lref l a

-- | Returns the current value of the thread's label.
currentLabel :: (Label l) => LIO l s l
currentLabel = get >>= return . lioL

-- | Returns the current value of the thread's clearance.
currentClearance :: (Label l) => LIO l s l
currentClearance = get >>= return . lioC

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
     @not (lcurrent ``leq`` ccurrent)@, then 'taint' throws an
     exception rather than raising the current label.

   * When /writing/ an object, it should be the case that @ldata
     ``leq`` lcurrent && lcurrent ``leq`` ldata@.  (As stated, this is
     the same as saying @ldata == lcurrent@, but the two are different
     when using 'leqp' instead of 'leq'.)  This is ensured by the
     'wguard' (write guard) function, which does the equivalent of
     'taint' to ensure the target label @ldata@ can flow to the
     current label, then throws an exception if @ldata@ cannot flow
     back to the target label.

   * When /creating/ or /allocating/ objects, it is permissible for
     them to be higher than the current label, so long as they are
     bellow the current clearance.  In other words, it must be the
     case that @lcurrent ``leq`` ldata && ldata ``leq`` ccurrent@.
     This is ensured by the 'aguard' (allocation guard) function.

The 'taintP' and 'wguardP' functions are variants of the above that
take privilege to be more permissive and raise the current label less.
There is no 'aguardP' function because it has not been necessary thus
far--the default clearance is usually high enough that when people
lower it they don't want it bypassed.

-}

-- |Use @taint l@ in trusted code before observing an object labeled
-- @l@.  This will raise the current label to a value @l'@ such that
-- @l ``leq`` l'@, or throw @'LerrClearance'@ if @l'@ would have to be
-- higher than the current clearance.
taint    :: (Label l) => l -> LIO l s ()
taint l' = do s <- get
              let l = lioL s `lub` l'
              if l `leq` lioC s
                then put s { lioL = l }
                else throwIO LerrClearance

-- |Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that unlike 'setLabelP', @taintP@ will never lower
-- the current label.  It simply uses privileges to avoid raising the
-- label as high as 'taint' would raise it.
taintP      :: (Priv l p) =>
                 p              -- ^Privileges to invoke
              -> l              -- ^Label to taint to if no privileges
              -> LIO l s ()
taintP p l' = do s <- get
                 let l = lostar p l' (lioL s)
                 if l `leq` lioC s
                   then put s { lioL = l }
                   else throwIO LerrClearance

-- |Use @wguard l@ in trusted code before modifying an object labeled
-- @l@.  If @l'@ is the current label, then this function ensures that
-- @l' ``leq`` l@ before doing the same thing as @'ltaintio' l@.
-- Throws @'LerrHigh'@ if the current label @l'@ is too high.
wguard :: (Label l) => l -> LIO l s ()
wguard l = do l' <- currentLabel
              if l' `leq` l
               then taint l
               else throwIO LerrHigh

-- |Like 'wguard', but takes privilege argument to be more permissive.
wguardP     :: (Priv l p) => p -> l -> LIO l s ()
wguardP p l = do l' <- currentLabel
                 if leqp p l' l
                  then taintP p l
                  else throwIO LerrHigh

-- | Ensures the label argument is between the current IO label and
-- current IO clearance.  Use this function in code that allocates
-- objects--you shouldn't be able to create an object labeled @l@
-- unless @aguard l@ does not throw an exception.
aguard :: (Label l) => l -> LIO l s ()
aguard newl = do c <- currentClearance
                 l <- currentLabel
                 unless (leq newl c) $ throwIO LerrClearance
                 unless (leq l newl) $ throwIO LerrLow
                 return ()

-- | If the current label is @oldLabel@ and the current clearance is
-- @clearance@, this function allows a code to raise the label to any
-- value @newLabel@ such that
-- @oldLabel ``leq`` newLabel && newLabel ``leq`` clearance@.
-- Note that there is no @setLabel@ variant without the @...P@ because
-- the 'taint' function provides essentially the same functionality
-- that @setLabel@ would.
setLabelP     :: (Priv l p) => p -> l -> LIO l s ()
setLabelP p l = do s <- get
                   if leqp p (lioL s) l
                     then put s { lioL = l }
                     else throwIO LerrPriv

setLabelTCB     :: (Label l) => l -> LIO l s ()
setLabelTCB l = do s <- get
                   if l `leq` lioC s
                      then put s { lioL = l }
                      else throwIO LerrClearance

-- |Reduce the current clearance.  One cannot raise the current label
-- or create object with labels higher than the current clearance.
setClearance   :: (Label l) => l -> LIO l s ()
setClearance l = get >>= doit
    where doit s | not $ l `leq` lioC s = throwIO LerrClearance
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = put s { lioC = l }

-- |Raise the current clearance (undoing the effects of 'setClearance').
-- This requires privileges.
setClearanceP   :: (Priv l p) => p -> l -> LIO l s ()
setClearanceP p l = get >>= doit
    where doit s | not $ leqp p l $ lioC s = throwIO LerrPriv
                 | not $ lioL s `leq` l = throwIO LerrLow
                 | otherwise            = put s { lioC = l }

setClearanceTCB   :: (Label l) => l -> LIO l s ()
setClearanceTCB l = get >>= doit
    where doit s | not $ lioL s `leq` l = throwIO LerrInval
                 | otherwise            = put s { lioC = l }

-- |Within the 'LIO' monad, this function takes an 'Lref' and returns
-- the value.  Thus, in the 'LIO' monad one can say:
--
-- > x <- openR (xref :: Lref SomeLabelType Int)
--
-- And now it is possible to use the value of @x@, which is the pure
-- value of what was stored in @xref@.  Of course, @openR@ also raises
-- the current label.  If raising the label would exceed the current
-- clearance, then the value of @x@ is undefined, which means the
-- value of any computation that actually uses @x@ will also be
-- undefined.
openR             :: (Label l) => Lref l a -> LIO l s a
openR (Lref la a) = do
  s <- get
  if la `leq` lioC s
    then do put s { lioL = lioL s `lub` la }
            return a
    else
        return undefined

-- Might have lowered clearance inside closeR, so just preserve it
-- |@closeR@ is the dual of @openR@.  It allows one to invoke
-- computations that would raise the current label, but without
-- actually raising the label.  Instead, the result of the computation
-- is packaged into an 'Lref'.  Thus, to get at the result of the
-- computation one will have to call 'openR' and raise the label, but
-- this can be postponed, or done inside some other call to 'closeR'.
closeR   :: (Label l) => LIO l s a -> LIO l s (Lref l a)
closeR m = do
  LIOstate { lioL = l, lioC = c } <- get
  a <- m
  s <- get
  put s { lioL = l, lioC = c }
  return $ Lref (lioL s) a

-- |Executes a computation that would raise the current label, but
-- discards the result so as to keep the label the same.
discardR   :: (Label l) => LIO l s a -> LIO l s ()
discardR m = closeR m >> return ()
  

getTCB :: (Label l) => LIO l s s
getTCB = get >>= return . labelState

putTCB    :: (Label l) => s -> LIO l s ()
putTCB ls = get >>= put . update
    where update s = s { labelState = ls }

newstate   :: (Label l) => s -> LIOstate l s
newstate s = LIOstate { labelState = s , lioL = lpure , lioC = lclear }

mkLIO :: (Label l) => (LIOstate l s -> IO (a, LIOstate l s)) -> LIO l s a
mkLIO = LIO . StateT

unLIO                  :: (Label l) => LIO l s a -> LIOstate l s
                       -> IO (a, LIOstate l s)
unLIO (LIO (StateT f)) = f

runLIO     :: forall l s a. (Label l) => LIO l s a -> LIOstate l s
           -> IO (a, LIOstate l s)
runLIO m s = unLIO m s `E.catch` (E.throwIO . delabel)
    where delabel :: LabeledExceptionTCB l -> SomeException
          delabel (LabeledExceptionTCB l e) = e
           -- trace ("unlabeling " ++ show e ++ " {" ++ show l ++ "}") e

evalLIO     :: (Label l) => LIO l s a -> s -> IO (a, l)
evalLIO m s = do (a, ls) <- runLIO m (newstate s)
                 return (a, lioL ls)

ioTCB :: (Label l) => IO a -> LIO l s a
ioTCB a = mkLIO $ \s -> do r <- a; return (r, s)

rtioTCB :: (Label l) => IO a -> LIO l s a
rtioTCB a = rethrowTCB $ mkLIO $ \s -> do r <- a; return (r, s)

iotTCB     :: (Label l) =>
              (IO (a, LIOstate l s) -> IO (a, LIOstate l s)) -> LIO l s a
           -> LIO l s a
iotTCB f m = mkLIO $ \s -> f (unLIO m s)


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
iomaps     :: (Label l) =>
              ((LIOstate l s) -> IO (a, LIOstate l s) -> IO (a, LIOstate l s))
           -> LIO l s a -> LIO l s a
iomaps f c = mkLIO $ \s -> (f s $ unLIO c s)

-- | Like 'iomaps' if you don't need the state.
iomap   :: (Label l) =>
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
   wrapped by 'discardR', one might examine a secret bit, and throw an
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
   downgrade the exception.  Note that privileged code that must
   always run some cleanup function can use the 'onExceptionTCB' and
   'bracketTCB' to run the cleanup code on all exceptions.

   Note:  Do not use the 'throw' (as opposed to 'throwIO') function
   within the 'LIO' monad.  Because 'throw' can be invoked from pure
   code, it has no notion of current label and so cannot assign an
   appropriate label to the exception.

-}

instance Label l => Show (LabeledExceptionTCB l) where
    showsPrec _ (LabeledExceptionTCB l e) rest =
        shows e $ (" {" ++) $ shows l $ "}" ++ rest

instance (Label l) => Exception (LabeledExceptionTCB l)

class (Monad m) => MonadCatch m where
    throwIO             :: (Exception e) => e -> m a
    catch               :: (Exception e) => m a -> (e -> m a) -> m a
    onException         :: m a -> m b -> m a
    onException io what = io `catch` \e ->
                          what >> throwIO (e :: SomeException)

instance MonadCatch IO where
    throwIO = E.throwIO
    catch = E.catch
    onException = E.onException

instance (Label l) => MonadCatch (LIO l s) where
    -- |It is not possible to catch pure exceptions from within the 'LIO'
    -- monad, but @throwIO@ wraps up an exception with the current label,
    -- so that it can be caught with 'catch' or 'catchP'..
    throwIO e = mkLIO $ \s -> E.throwIO $
                LabeledExceptionTCB (lioL s) (toException e)
    -- | Basic function for catching labeled exceptions.  (The fact that
    -- they are labeled is hidden from the handler.)
    --
    -- > catchL m c = catchP m NoPrivs (\_ -> c)
    --
    catch m c = iomaps (\s m' -> m' `E.catch` doit s) m
        where
          doit s e@(LabeledExceptionTCB l se) =
              case fromException se of
                Just e' | l `leq` lioL s -> unLIO (c e') s
                Nothing -> E.throwIO e



-- | Catches an exception, so long as the label at the point where the
-- exception was thrown can flow to the label at which catchP is
-- invoked, modulo the privileges specified.  Note that the handler
-- receives an an extra first argument (before the exception), which
-- is the label when the exception was thrown.
catchP       :: (Label l, Exception e, Priv l p) =>
                 LIO l s a             -- ^ Computation to run
             -> p   -- ^ Privileges with which to downgrade exception
             -> (l -> e -> LIO l s a) -- ^ Exception handler
             -> LIO l s a             -- ^ Result of computation or handler
catchP m p c = iomaps (\s m' -> m' `E.catch` doit s) m
    where doit s e@(LabeledExceptionTCB l se) =
              case fromException se of
                Just e' | leqp p l $ lioL s -> unLIO (c l e') s
                Nothing -> E.throw e

-- | 'onException' cannot run its handler if the label was raised in
-- the computation that threw the exception.  This variant allows
-- privileges to be supplied, so as to catch exceptions thrown with a
-- raised label.
onExceptionP           :: (Priv l p) =>
                          LIO l s a -- ^ The computation to run
                       -> p         -- ^ Privileges to downgrade exception
                       -> LIO l s b -- ^ Handler to run on exception
                       -> LIO l s a -- ^ Result if no exception thrown
onExceptionP io p what = catchP io p
                         (\l e -> what >> throwIO (e :: SomeException))

-- | @MonadBlock@ is the class of monads that support the 'block' and
-- 'unblock' functions for disabling and enabling asynchronous
-- exceptions, respectively.
class (Monad m) => MonadBlock m where
    block   :: m a -> m a
    unblock :: m a -> m a
instance MonadBlock IO where
    block   = E.block
    unblock = E.unblock
instance (Label l) => MonadBlock (LIO l s) where
    block   = iomap E.block
    unblock = iomap E.unblock

-- | For privileged code that needs to catch all exceptions in some
-- cleanup function.  Note that for the 'LIO' monad, this function
-- does /not/ call 'rethrowTCB' to label the exceptions.  It is
-- assumed that you will use 'rtioTCB' for IO within the computation.
class (MonadBlock m) => OnExceptionTCB m where
    onExceptionTCB :: m a -> m b -> m a
    bracketTCB     :: m a -> (a -> m b) -> (a -> m c) -> m c
    bracketTCB before after between =
        block $ do
          a <- before
          b <- unblock (between a) `onExceptionTCB` after a
          after a
          return b
instance OnExceptionTCB IO where
    onExceptionTCB = E.onException
    bracketTCB     = E.bracket
instance (Label l) => OnExceptionTCB (LIO l s) where
    onExceptionTCB io cleanup = 
        mkLIO $ \s -> unLIO io s
                      `E.catch` \e -> do unLIO cleanup s
                                         E.throwIO (e :: SomeException)

