{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables,
             ConstraintKinds,
             FlexibleContexts,
             DeriveDataTypeable #-}

{- | 

This module implements the core of the Labeled IO (LIO) library for
information flow control (IFC) in Haskell.  It provides a monad,
'LIO', that is intended to be used as a replacement for the 'IO' monad
in untrusted code.  The idea is for untrusted code to provide a
computation in the 'LIO' monad, which trusted code can then safely
execute through using 'evalLIO'-like functions. Though, usually a
wrapper function is employed depending on the type of /labels/ used by
an application.  For example, with "LIO.DCLabel" trusted code would
'evalDC' to execute an untrusted computation.

Labels are a way of describing who can observe and modify data. (A
detailed consideration of labels is given in "LIO.Label".) LIO
associates a /current label/ with every 'LIO' computation. The current
label effectively tracks the sensitivity of all the data that the
computation has observed.  For example, if the computation has read a
\"secret\" mutable refernce (see "LIO.LIORef") and then the result of
a \"top-secret\" thread (see "LIO.Concurrent") then the current label
will be at least \"top-secret\". The role of the current label is
two-fold. First, the current label protects all the data in scope --
it is the label associated with any /unlabeled/ data. For example, the
current label is the label on contants such as @3@ or @\"tis a
string\"@. More interestingly, consider reading a \"secret\" file:

> bs <- readFile "/secret/file.txt"

Though the label in the file store may be \"secret\", @bs@ has type
@ByteString@, which is not explicitly labeled. Hence, to protect the
contents (@bs@) the current label must be at least \"secret\" before
executing @readFile@.  More generally, if the current label is
@L_cur@, then it is only permissible to read data labeled @L_r@ if
@L_r ``canFlowTo`` L_cur@.  Note that, rather than throw an exception,
reading data will often just increase the current label to ensure that
@L_r ``canFlowTo`` L_cur@ using 'taint'.

Second, the current label prevents inforation leaks into public
channels. Specifically, it is only permissible to modify, or write
to, data labeled @L_w@ when @L_cur``canFlowTo`` L_w@. Thus, it the
following attempt to leak the secret @bs@ would fail:

> writeFile "/public/file.txt" bs

In addition to the current label, the LIO monad keeps a second label,
the current /clearance/ (accessible via the 'getClearance' function).
The clearance can be used to enforce a \"need-to-know\" policy since
it represents the highest value the current label can be raised to.
In other words, if the current clearance is @L_clear@ then the
computation cannot create, read or write to objects labeled @L@ such
that @L ``canFlowTo`` L_clear@ does not hold.

This module exports the 'LIO' monad, functions to access the internal
state (e.g., 'getLabel' and 'getClearance'), functions for raising and
catching exceptions, and IFC guards.  Exceptions are core to LIO since
they provide a way to deal with potentially-misbehaving untrusted
code. Specifically, when a computation is about to violate IFC (as
@writeFile@ above), an exception is raised. Guards provide a useful
abstraction for dealing with labeled objects; they should be used
before performing a read-only, write-only, or read-write operation on
a labeled object. The remaining core, but not all, abstractions are
exported by "LIO".

-}

module LIO.Core (
  -- * LIO monad
    LIO
  , MonadLIO(..), MonadControlLIO
  -- ** Execute LIO actions
  , evalLIO, runLIO, tryLIO, paranoidLIO
  -- ** Internal state
  , LIOState(..)
  -- *** Current label
  , getLabel, setLabel, setLabelP
  -- *** Current clerance
  , getClearance, setClearance, setClearanceP
  , withClearance, withClearanceP
  -- * Exceptions
  -- $exceptions
  , LabeledException
  -- ** Throwing exceptions
  , throwLIO
  -- ** Catching exceptions
  , catchLIO, catchLIOP
  -- ** Utilities
  -- $utils
  , onException, onExceptionP
  , finally, finallyP
  , bracket, bracketP
  , evaluate
  -- ** Exceptions thrown by LIO
  -- $lioExceptions
  , MonitorFailure(..)
  , VMonitorFailure(..)
  -- * Guards
  -- $guards
  -- ** Allocate/write-only
  , guardAlloc, guardAllocP
  -- ** Read-only
  , taint, taintP
  -- ** Write
  , guardWrite, guardWriteP
  ) where

import           Prelude hiding (catch)
import           Data.Typeable

import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Trans.State.Strict
import qualified Control.Exception as E
import           Control.Exception hiding ( finally
                                          , onException
                                          , bracket
                                          , evaluate )

import           LIO.TCB
import           LIO.Label
import           LIO.Privs


--
-- Execute LIO actions
--

-- | Given an 'LIO' computation and some initial state, return an
-- IO action which when executed will perform the IFC-safe LIO
-- computation.
--
-- Because untrusted code cannot execute 'IO' computations, this function
-- should only be useful within trusted code.  No harm is done from
-- exposing the @evalLIO@ symbol to untrusted code.  (In general,
-- untrusted code is free to produce 'IO' computations, but it cannot
-- execute them.)
evalLIO :: Label l
        => LIO l a       
        -- ^ LIO computation
        -> LIOState l
        -- ^ Initial state
        -> IO a
evalLIO act s = fst `liftM` runLIO act s

-- | Execute an 'LIO' action, returning the final state.
-- See 'evalLIO'.
runLIO :: Label l
       => LIO l a
        -- ^ LIO computation that may throw an exception
       -> LIOState l
        -- ^ Initial state
       -> IO (a, LIOState l)
runLIO act s = runStateT (unLIOTCB act) s

-- | Similar to 'evalLIO', but catches all exceptions, including
-- language level exceptions.
paranoidLIO :: Label l
            => LIO l a
             -- ^ LIO computation that may throw an exception
            -> LIOState l
             -- ^ Initial state
            -> IO (Either SomeException (a, LIOState l))
paranoidLIO act s = (Right `liftM` runLIO act s) `catch` (return . Left)

-- | Similar to 'evalLIO', but catches all exceptions exceptions
-- thrown with 'throwLIO'.
tryLIO :: Label l
       => LIO l a
        -- ^ LIO computation that may throw an exception
       -> LIOState l
        -- ^ Initial state
       -> IO (Either (LabeledException l) a, LIOState l)
tryLIO act = runLIO (Right `liftM` act `catchTCB` (return . Left))

--
-- Internal state
--

-- | Returns the current value of the thread's label.
getLabel :: MonadLIO l m => m l
getLabel = liftLIO $ lioLabel `liftM` getLIOStateTCB


-- | Raise the current label to the provided label, which must be
-- between the current label and clearance. See 'taint'.
setLabel :: MonadLIO l m => l -> m ()
setLabel = setLabelP NoPrivs

-- | If the current label is @oldLabel@ and the current clearance is
-- @clearance@, this function allows code to raise the current label to
-- any value @newLabel@ such that @oldLabel ``canFlowTo`` newLabel &&
-- newLabel ``canFlowTo`` clearance@.
setLabelP :: (MonadLIO l m, Priv l p) => p -> l -> m ()
setLabelP p l = do
  liftLIO $ guardAllocP p l `catchLIO`
      \(_ :: MonitorFailure) -> throwLIO InsufficientPrivs
  liftLIO . updateLIOStateTCB $ \s -> s { lioLabel = l }

-- | Returns the current value of the thread's clearance.
getClearance :: MonadLIO l m => m l
getClearance = liftLIO $ lioClearance `liftM` getLIOStateTCB

-- | Lower the current clearance. The new clerance must be between
-- the current label and clerance. One cannot raise the current label
-- or create object with labels higher than the current clearance.
setClearance :: MonadLIO l m => l -> m ()
setClearance = setClearanceP NoPrivs

-- | Raise the current clearance (undoing the effects of
-- 'setClearance') by exercising privileges. If the current label is
-- @l@ and current clearance is @c@, then @setClearanceP p cnew@
-- succeeds only if the new clearance is can flow to the current
-- clearance (modulo privileges), i.e., @'canFlowToP' p cnew c@ must
-- hold. Additionally, the current label must flow to the new
-- clearance, i.e., @l ``canFlowTo`` cnew@ must hold.
setClearanceP :: (MonadLIO l m, Priv l p) => p -> l -> m ()
setClearanceP p cnew = do
  l <- getLabel
  c <- getClearance
  unless (canFlowToP p cnew c) $! throwLIO InsufficientPrivs
  unless (l `canFlowTo` cnew)  $! throwLIO CurrentLabelViolation
  liftLIO . updateLIOStateTCB $ \s -> s { lioClearance = cnew }

-- | Lowers the clearance of a computation, then restores the clearance
-- to its previous value.  Useful to wrap around a computation if you
-- want to be sure you can catch exceptions thrown by it. The supplied
-- clearance label must be bounded by the current label and clearance
-- as enforced by 'guardAlloc'.
-- 
-- Note that if the computation inside @withClearance@ acquires any
-- 'Priv's, it may still be able to raise its clearance above the
-- supplied argument using 'setClearanceP'.
withClearance :: MonadControlLIO l m => l -> m a -> m a
withClearance = withClearanceP NoPrivs

-- | Same as 'withClearance', but uses privileges when applying
-- 'guardAllocP' to the supplied label.
withClearanceP :: (MonadControlLIO l m, Priv l p) => p -> l -> m a -> m a
withClearanceP p l act = do
  guardAllocP p l
  c <- getClearance
  liftLIO . updateLIOStateTCB $ \s -> s { lioClearance = l }
  act `finally` (liftBase . updateLIOStateTCB $ \s ->
                               s { lioClearance = c  })

--
-- Exceptions
--

{- $exceptions

   We must define 'throwIO'- and 'catch'-like functions to work in
   the 'LIO' monad.  A complication is that exceptions could
   potentially leak information.  For instance, one might examine a
   secret bit, and throw an exception when the bit is 1 but not 0.
   Allowing untrusted code to catch the exception leaks the bit.

   The solution is to wrap exceptions up with a label.  The exception
   may be caught, but only if the label of the exception can flow to
   the label at the point the catch statement began execution.
   Arbitrary code can use 'throwLIO' to throw an exception that will
   be labeled with the current label, while 'catchLIO' can be used to
   catch exceptions (whose label flows to the current clearance).
   Wherever possible, code should use the 'catchLIOP' and
   'onExceptionP' variants that use privileges to downgrade the
   exception. 

   If an exception is uncaught in the 'LIO' monad, the 'evalLIO'
   function will re-throw the exception, so that it is okay to throw
   exceptions from within the 'LIO' monad and catch them within the
   'IO' monad.  Of course, code in the 'IO' monad must be careful not
   to let the 'LIO' code exploit it to exfiltrate information.  Hence,
   we recommend the use of 'paranoidLIO' to execute 'LIO' actions as
   to prevent accidental, but unwanted crashes.


   /Note/:  Do not use 'throw' (as opposed to 'throwLIO') within the
   'LIO' monad.  Because 'throw' can be invoked from pure code, it has
   no notion of current label and so cannot assign an appropriate
   label to the exception.  As a result, the exception will not be
   catchable within the 'LIO' monad and will propagate all the way out
   to the executing 'IO' layer.  Similarly, asynchronous exceptions
   (such as divide by zero or undefined values in lazily evaluated
   expressions) cannot be caught within the 'LIO' monad.

-}

--
-- Throwing exceptions
--

-- | Throw an exception. The label on the exception is the current
-- label.
throwLIO :: (Exception e, MonadLIO l m) => e -> m a
throwLIO e = do
  l <- getLabel
  liftLIO . unlabeledThrowTCB $! LabeledExceptionTCB l (toException e)

--
-- Catching exceptions
--


-- | Same as 'catchLIO' but does not use privileges when raising the
-- current label to the join of the current label and exception label.
catchLIOP :: (Exception e, MonadControlLIO l m, Priv l p)
          => p
          -> m a
          -> (e -> m a)
          -> m a
catchLIOP p act handler = do 
  clr <- getClearance
  act `catchTCB` \se@(LabeledExceptionTCB l seInner) -> 
    case fromException seInner of
     Just e | l `canFlowTo` clr -> taintP p l >> handler e
     _                          -> liftLIO $ unlabeledThrowTCB se

-- | Catches an exception, so long as the label at the point where the
-- exception was thrown can flow to the clearance at which @catchLIO@ is
-- invoked. Note that the handler raises the current label to the join
-- ('upperBound') of the current label and exception label.
catchLIO :: (Exception e, MonadControlLIO l m)
         => m a
         -> (e -> m a)
         -> m a
catchLIO = catchLIOP NoPrivs 

--
-- Utilities
--

{- $utils

Similar to "Control.Exception" we export 'onException', 'finally' and
'bracket' which should be used by programmers to properly acquire and
release resources in the presence of exceptions.  Different from
"Control.Exception" our non-TCB utilities are not implemented in terms
of 'mask', and thus untrusted threads may be killed (and thus garbage
collected) with synchronous exceptions. Of course, only trusted code
may has access to 'throwTo'-like functionality.

-}

-- | Performs an action only if there was an exception raised by the
-- computation. Note that the exception is rethrown after the final
-- computation is executed.
onException :: (MonadControlLIO l m)
            => m a -- ^ The computation to run
            -> m b -- ^ Computation to run on exception
            -> m a -- ^ Result if no exception thrown
onException = onExceptionP NoPrivs

-- | Privileged version of 'onExceptionP'.  'onException' cannot run its
-- handler if the label was raised in the computation that threw the
-- exception.  This variant allows privileges to be supplied, so as to
-- catch exceptions thrown with a \"higher\" label.
onExceptionP :: (MonadControlLIO l m, Priv l p)
             => p       -- ^ Privileges to downgrade exception
             -> m a -- ^ The computation to run
             -> m b -- ^ Computation to run on exception
             -> m a -- ^ Result if no exception thrown
onExceptionP p act1 act2 = 
    catchLIOP p act1 (\(e :: SomeException) -> do
                       void act2
                       throwLIO e )

-- | Execute a computation and a finalizer, which is executed even if
-- an exception is raised in the first computation.
finally :: (MonadControlLIO l m)
        => m a -- ^ The computation to run firstly
        -> m b -- ^ Final computation to run (even if exception is thrown)
        -> m a -- ^ Result of first action
finally = finallyP NoPrivs

-- | Version of 'finally' that uses privileges when handling
-- exceptions thrown in the first computation.
finallyP :: (MonadControlLIO l m, Priv l p)
         => p   -- ^ Privileges to downgrade exception
         -> m a -- ^ The computation to run firstly
         -> m b -- ^ Final computation to run (even if exception is thrown)
         -> m a -- ^ Result of first action
finallyP p act1 act2 = do
  r <- onExceptionP p act1 act2
  void act2
  return r

-- | The @bracket@ function is used in patterns where you acquire a
-- resource, perform a computation on it, and then release the resource.
-- The function releases the resource even if an exception is raised in
-- the computation. An example of its use case is file handling:
--
-- >  bracket
-- >    (openFile ... {- open file -} )
-- >    (\handle -> {- close file -} )
-- >    (\handle -> {- computation on handle -})
--
-- Note: @bracket@ does not use 'mask' and thus asynchronous may leave
-- the resource unreleased if the thread is killed in during release.
-- An interface for arbitrarily killing threads is not provided by LIO.
bracket :: (MonadControlLIO l m)
        => m a           -- ^ Computation to run first
        -> (a -> m c)    -- ^ Computation to run last
        -> (a -> m b)    -- ^ Computation to run in-between
        -> m b
bracket = bracketP NoPrivs

-- | Like 'bracket', but uses privileges to downgrade the label of any
-- raised exception.
bracketP :: (MonadControlLIO l m, Priv l p)
         => p             -- ^ Priviliges used to downgrade
         -> m a           -- ^ Computation to run first
         -> (a -> m c)    -- ^ Computation to run last
         -> (a -> m b)    -- ^ Computation to run in-between
         -> m b
bracketP p first third second = do
  x <- first
  finallyP p (second x) (third x)

-- | Forces its argument to be evaluated to weak head normal form when the
-- resultant LIO action is executed. This is simply a wrapper for 
-- "Control.Exception"'s @evaluate@.
evaluate :: MonadLIO l m => a -> m a
evaluate = liftLIO . rethrowIoTCB . E.evaluate

--
-- Exceptions thrown by LIO
--

{- $lioExceptions

Library functions throw an exceptions before an IFC violation can take
place. 'MonitorFailure' should be used when the reason for failure is
sufficiently described by the type. Otherwise, 'VMonitorFailure'
(i.e., \"Verbose\"-'MonitorFailure') should be used to further
describe the error.

-}

-- | Exceptions thrown when some IFC restriction is about to be
-- violated.
data MonitorFailure = ClearanceViolation
                    -- ^ Current label would exceed clearance, or
                    -- object label is above clearance.
                    | CurrentLabelViolation
                    -- ^ Clearance would be below current label, or
                    -- object label is not above current label.
                    | InsufficientPrivs
                    -- ^ Insufficient privileges. Thrown when lowering
                    -- the current label or raising the clearance
                    -- cannot be accomplished with the supplied
                    -- privileges.
                    | CanFlowToViolation
                    -- ^ Generic can-flow-to failure, use with
                    -- 'VMonitorFailure'
                    deriving (Show, Typeable)

instance Exception MonitorFailure

-- | Verbose version of 'MonitorFailure' also carrying around a
-- detailed message.
data VMonitorFailure = VMonitorFailure { monitorFailure :: MonitorFailure
                                       -- ^ Generic monitor failure.
                                       , monitorMessage :: String
                                       -- ^ Detailed message of failure.
                                       }
                    deriving Typeable

instance Show VMonitorFailure where
  show m = (show $ monitorFailure m) ++ ": " ++ (monitorMessage m)

instance Exception VMonitorFailure


--
-- Guards
--

{- $guards

   Guards are used by (usually privileged) code to check that the
   invoking, unprivileged code has access to particular data.  If the
   current label is @lcurrent@ and the current clearance is
   @ccurrent@, then the following checks should be performed when
   accessing data labeled @ldata@:

   * When /reading/ an object labeled @ldata@, it must be the case
     that @ldata ``canFlowTo`` lcurrent@.  This check is performed by
     the 'taint' function, so named because it \"taints\" the current
     'LIO' context by raising @lcurrent@ until @ldata ``canFlowTo``
     lcurrent@.  (Specifically, it does this by computing the
     least 'upperBound' of the two labels.) However, this is done
     only if the new @lcurrent ``canFlowTo`` ccurrent@.

   * When /creating/ or /allocating/ objects, it is permissible for
     them to be higher than the current label, so long as they are
     below the current clearance.  In other words, it must be the
     case that @lcurrent ``canFlowTo`` ldata && ldata ``canFlowTo``
     ccurrent@.  This is ensured by the 'guardAlloc' function.

   * When /writing/ an object, it should be the case that
     @lcurrent ``canFlowTo`` ldata && ldata ``canFlowTo`` lcurrent@.
     (As stated, this is the same as saying @ldata == lcurrent@, but
     the two are different when using 'canFlowToP' instead of
     'canFlowTo'.) This is ensured by the 'guardWrite' function, which
     does the equivalent of 'taint' to ensure the target label @ldata@
     can flow to the current label, then throws an exception if
     @lcurrent@ cannot flow back to the target label.

     Note that in this case a write /always/ implies a read. Hence,
     when writing to an object for which you can observe the result,
     you must use 'guardWrite'. However, when performing a write for
     which there is no observable side-effects to the writer, i.e.,
     you cannot observe the success or failure of the write, then it
     is safe to solely use 'guardAlloc'.


The 'taintP', 'guardAllocP',  and 'guardWriteP' functions are variants
of the above that take privilege to be more permissive and raise the
current label less. 

-}

--
-- Allocation
--

-- | Ensures the label argument is between the current IO label and
-- current IO clearance.  Use this function in code that allocates
-- objects--untrusted code shouldn't be able to create an object
-- labeled @l@ unless @guardAlloc l@ does not throw an exception.
-- Similarly use this guard in any code that writes to an
-- object labeled @l@ for which the write has no observable
-- side-effects.
--
-- If the label does not flow to clearance 'ClearanceViolation' is
-- thrown; if the current label does not flow to the argument label
-- 'CurrentLabelViolation' is thrown.
guardAlloc :: MonadLIO l m => l -> m ()
guardAlloc = guardAllocP NoPrivs

-- | Like 'guardAlloc', but takes privilege argument to be more
-- permissive.  Note: privileges are /only/ used when checking that
-- the current label can flow to the given label.
guardAllocP :: (MonadLIO l m, Priv l p) => p -> l -> m ()
guardAllocP p newl = do
  c <- getClearance
  l <- getLabel
  unless (canFlowToP p l newl) $! throwLIO CurrentLabelViolation
  unless (newl `canFlowTo` c)  $! throwLIO ClearanceViolation

--
-- Read
--

-- | Use @taint l@ in trusted code before observing an object labeled
-- @l@.  This will raise the current label to a value @l'@ such that
-- @l ``canFlowTo`` l'@, or throw 'ClearanceViolation' if @l'@ would
-- have to be higher than the current clearance.
taint :: MonadLIO l m => l -> m ()
taint = taintP NoPrivs

-- | Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that @taintP@ will never lower the current label.
-- It simply uses privileges to avoid raising the label as high as
-- 'taint' would raise it.
taintP :: (MonadLIO l m, Priv l p) => p -> l -> m ()
taintP p newl = do
  c <- getClearance
  l <- getLabel
  let l' = partDowngradeP p newl l
  unless (l' `canFlowTo` c) $! throwLIO ClearanceViolation
  liftLIO . updateLIOStateTCB $ \s -> s { lioLabel = l' }


-- | Use @guardWrite l@ in any (trusted) code before modifying an
-- object labeled @l@, for which a the modification can be observed,
-- i.e., the write implies a read.
--
-- The implementation of @guardWrite@ is straight forward:
--
-- > guardWrite l = taint l >> guardAlloc l
--
-- This guarantees that @l@ ``canFlowTo`` the current label (and
-- clearance), and that the current label ``canFlowTo`` @l@.
--
guardWrite :: MonadLIO l m => l -> m ()
guardWrite = guardWriteP NoPrivs

-- | Like 'guardWrite', but takes privilege argument to be more
-- permissive.
guardWriteP ::(MonadLIO l m, Priv l p) => p -> l -> m ()
guardWriteP p newl = do
  taintP      p newl
  guardAllocP p newl
