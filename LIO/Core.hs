{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | 

LIO core
-}

module LIO.Core (
  -- * LIO monad
    LIO
  -- ** Execute LIO actions
  , evalLIO, runLIO, tryLIO
  -- ** Internal state
  , LIOState(..), defaultState
  -- *** Current label
  , getLabel, setLabelP
  -- *** Current clerance
  , getClearance, lowerClearance, setClearanceP
  -- * Exceptions
  -- $exceptions
  , LabeledException
  -- ** Throwing exceptions
  , throwLIO, throwIO
  -- ** Catching exceptions
  , catchLIO, catch, catchLIOP
  -- ** Utilities
  -- $utils
  , onException, onExceptionP
  , finally, finallyP
  , bracket, bracketP
  , evaluate
  -- * Guards
  -- $guards
  -- ** Allocate/write-only
  , guardAlloc, guardAllocP
  -- ** Read-only
  , taint, taintP
  -- ** Write
  , guardWrite, guardWriteP
  ) where

import           Prelude hiding ( catch )
import           Control.Monad.State.Strict
import qualified Control.Exception as E
import           Control.Exception hiding ( throwIO
                                          , catch
                                          , finally
                                          , onException
                                          , bracket
                                          , evaluate )

import           LIO.TCB
import           LIO.Label
import           LIO.Privs
import           LIO.Exception

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
evalLIO act s = evalStateT (unLIOTCB act) s

-- | Execute an 'LIO' action, returning the final state.
-- See 'evalLIO'.
runLIO :: Label l
       => LIO l a
        -- ^ LIO computation that may throw an exception
       -> LIOState l
        -- ^ Initial state
       -> IO (a, LIOState l)
runLIO act s = runStateT (unLIOTCB act) s

-- | Similar to 'evalLIO', but catches any exceptions thrown by
-- untrusted code instead of propagating them.
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

-- | Default 'LIO' state with the current label set to 'bottom' and
-- clearance set to 'top'.
defaultState :: Label l => LIOState l
defaultState = LIOState { lioLabel = bottom
                        , lioClearance = top }

-- | Returns the current value of the thread's label.
getLabel :: Label l => LIO l l
getLabel = lioLabel `liftM` getLIOStateTCB

-- | If the current label is @oldLabel@ and the current clearance is
-- @clearance@, this function allows code to raise the current label to
-- any value @newLabel@ such that @oldLabel ``canFlowTo`` newLabel &&
-- newLabel ``canFlowTo`` clearance@.
setLabelP :: Priv l p => p -> l -> LIO l ()
setLabelP p l = do
  guardAllocP p l `catchLIO`
    \(_ :: MonitorFailure) -> throwLIO InsufficientPrivs
  updateLIOStateTCB $ \s -> s { lioLabel = l }

-- | Returns the current value of the thread's clearance.
getClearance :: Label l => LIO l l
getClearance = lioClearance `liftM` getLIOStateTCB

-- | Lower the current clearance. The new clerance must be between
-- the current label and clerance. One cannot raise the current label
-- or create object with labels higher than the current clearance.
lowerClearance :: Label l => l -> LIO l ()
lowerClearance = setClearanceP NoPrivs

-- | Raise the current clearance (undoing the effects of
-- 'lowerClearance') by exercising privileges. If the current label is
-- @l@ and current clearance is @c@, then @setClearanceP p cnew@
-- succeeds only if the new clearance is can flow to the current
-- clearance (modulo privileges), i.e., @'canFlowToP' p cnew c@ must
-- hold. Additionally, the current label must flow to the new
-- clearance, i.e., @l ``canFlowTo`` cnew@ must hold.
setClearanceP :: Priv l p => p -> l -> LIO l ()
setClearanceP p cnew = do
  l <- getLabel
  c <- getClearance
  unless (canFlowToP p cnew c) $! throwLIO InsufficientPrivs
  unless (l `canFlowTo` cnew)  $! throwLIO CurrentLabelViolation
  updateLIOStateTCB $ \s -> s { lioClearance = cnew }

--
-- Exceptions
--

{- $exceptions

   We must re-define the 'throwIO' and 'catch' functions to work in
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

   If an exception is uncaught in the 'LIO' monad, the 'evalLIO'
   function will re-throw the exception, so that it is okay to throw
   exceptions from within the 'LIO' monad and catch them within the
   'IO' monad.  Of course, code in the 'IO' monad must be careful not
   to let the 'LIO' code exploit it to exfiltrate information.  Hence,
   we recommend the use of 'tryLIO' to execute 'LIO' actions as to
   prevent accidental, but unwanted crashes.

   Wherever possible, code should use the 'catchLIOP' and
   'onExceptionP' variants that use whatever privilege is available to
   downgrade the exception.  Privileged code that must always run some
   cleanup function can use the 'onExceptionTCB' and 'bracketTCB'
   functions to run the cleanup code on all exceptions.

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
throwLIO :: (Exception e, Label l) => e -> LIO l a
throwLIO e = do
  l <- getLabel
  unlabeledThrowTCB $! LabeledExceptionTCB l (toException e)

-- | For compatability we define 'throwIO' as 'throwLIO'
throwIO :: (Exception e, Label l) => e -> LIO l a
throwIO = throwLIO

--
-- Catching exceptions
--


-- | Catches an exception, so long as the label at the point where the
-- exception was thrown can flow to the label at which @catchLIOP@ is
-- invoked, modulo the privileges specified.  Note that the handler
-- raises the current label to the joint of the current label and
-- exception label.
catchLIOP :: (Exception e, Priv l p)
          => p
          -> LIO l a
          -> (e -> LIO l a)
          -> LIO l a
catchLIOP p act handler = do 
  clr <- getClearance
  act `catchTCB` \se@(LabeledExceptionTCB l seInner) -> 
    case fromException seInner of
     Just e | l `canFlowTo` clr -> taintP p l >> handler e
     _                          -> unlabeledThrowTCB se

-- | Same as 'catchLIOP' but does not use privileges when \"tainting\"
-- by exception label.
catchLIO :: (Exception e, Label l)
         => LIO l a
         -> (e -> LIO l a)
         -> LIO l a
catchLIO = catchLIOP NoPrivs 

-- | For compatability we define 'catch' as 'catchLIO'
catch :: (Exception e, Label l)
      => LIO l a
      -> (e -> LIO l a)
      -> LIO l a
catch = catchLIO

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
onException :: Label l
            => LIO l a -- ^ The computation to run
            -> LIO l b -- ^ Computation to run on exception
            -> LIO l a -- ^ Result if no exception thrown
onException = onExceptionP NoPrivs

-- | Privileged version of 'onExceptionP'.  'onException' cannot run its
-- handler if the label was raised in the computation that threw the
-- exception.  This variant allows privileges to be supplied, so as to
-- catch exceptions thrown with a raised label.
onExceptionP :: Priv l p
             => p       -- ^ Privileges to downgrade exception
             -> LIO l a -- ^ The computation to run
             -> LIO l b -- ^ Computation to run on exception
             -> LIO l a -- ^ Result if no exception thrown
onExceptionP p act1 act2 = 
    catchLIOP p act1 (\(e :: SomeException) -> do
                       void act2
                       throwIO e )

-- | Execute a computation and a finalizer, which is executed even if
-- an exception is raised in the first computation.
finally :: Label l
        => LIO l a -- ^ The computation to run firstly
        -> LIO l b -- ^ Final computation to run (even if exception is thrown)
        -> LIO l a -- ^ Result of first action
finally = finallyP NoPrivs

-- | Version of 'finally' that uses privileges when handling
-- exceptions thrown in the first computation.
finallyP :: Priv l p
         => p       -- ^ Privileges to downgrade exception
         -> LIO l a -- ^ The computation to run firstly
         -> LIO l b -- ^ Final computation to run (even if exception is thrown)
         -> LIO l a -- ^ Result of first action
finallyP p act1 act2 = do
  r <- onExceptionP p act1 act2
  void act2
  return r

-- | Like standard 'bracket', but with privileges to downgrade exception.
-- The @bracket@ function is used in patterns where you acquire a
-- resource, perform a computation on it, and then release the resource.
-- The function releases the resource even if an exception is raised in
-- the computation. An example of its use case is file handling:
--
-- >  bracket
-- >    (openFile ... {- open file -} )
-- >    (\handle -> {- close file -} )
-- >    (\handle -> {- computation on handle -})
--
-- Note: @bracket@ does not use mask and thus asynchronous may leave
-- the resource unreleased if the thread is killed in during release.
bracket :: Label l
        => LIO l a           -- ^ Computation to run first
        -> (a -> LIO l c)    -- ^ Computation to run last
        -> (a -> LIO l b)    -- ^ Computation to run in-between
        -> LIO l b
bracket = bracketP NoPrivs

-- | Like standard 'bracket', but with privileges to downgrade
-- exception.
bracketP :: Priv l p
         => p                 -- ^ Priviliges used to downgrade
         -> LIO l a           -- ^ Computation to run first
         -> (a -> LIO l c)    -- ^ Computation to run last
         -> (a -> LIO l b)    -- ^ Computation to run in-between
         -> LIO l b
bracketP p first third second = do
  x <- first
  finallyP p (second x) (third x)

-- | Forces its argument to be evaluated to weak head normal form when the
-- resultant LIO action is executed. This is simply a wrapper for 
-- "Control.Exception"'s @evaluate@.
evaluate :: Label l => a -> LIO l a
evaluate = rethrowIoTCB . E.evaluate


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
     lcurrent@.  (Specifically, it does this by computing the 'join'
     (least upper bound) of the two labels.) However, this is done
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
guardAlloc :: Label l => l -> LIO l ()
guardAlloc = guardAllocP NoPrivs

-- | Like 'guardAlloc', but takes privilege argument to be more
-- permissive.  Note: privileges are /only/ used when checking that
-- the current label can flow to the given label.
guardAllocP :: Priv l p => p -> l -> LIO l ()
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
taint :: Label l=> l -> LIO l ()
taint = taintP NoPrivs

-- | Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that @taintP@ will never lower the current label.
-- It simply uses privileges to avoid raising the label as high as
-- 'taint' would raise it.
taintP :: Priv l p => p -> l -> LIO l ()
taintP p newl = do
  c <- getClearance
  l <- getLabel
  let l' = labelDiffP p newl l
  unless (l' `canFlowTo` c) $! throwLIO ClearanceViolation
  updateLIOStateTCB $ \s -> s { lioLabel = l' }


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
guardWrite :: Label l => l -> LIO l ()
guardWrite = guardWriteP NoPrivs

-- | Like 'guardWrite', but takes privilege argument to be more
-- permissive.
guardWriteP :: Priv l p => p -> l -> LIO l ()
guardWriteP p newl = do
  taintP      p newl
  guardAllocP p newl
