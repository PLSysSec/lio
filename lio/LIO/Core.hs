{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
current label is the label on constants such as @3@ or @\"tis a
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
  , MonadLIO(..)
  -- ** Execute LIO actions
  , LIOState(..), evalLIO, runLIO
  -- ** Manipulating label state
  , getLabel, setLabel, setLabelP
  -- ** Manipulating clearance
  , getClearance, setClearance, setClearanceP
  , scopeClearance, withClearance, withClearanceP
  -- * Exceptions thrown by LIO
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


import qualified Control.Exception as IO
import Control.Monad
import Data.IORef
import Data.Typeable

import LIO.Exception
import LIO.TCB
import LIO.Label
import LIO.Privs
import LIO.Run


--
-- Internal state
--

-- | Returns the current value of the thread's label.
getLabel :: Label l => LIO l l
getLabel = lioLabel `liftM` getLIOStateTCB


-- | Raise the current label to the provided label, which must be
-- between the current label and clearance. See 'taint'.
setLabel :: Label l => l -> LIO l ()
setLabel = setLabelP noPrivs

-- | If the current label is @oldLabel@ and the current clearance is
-- @clearance@, this function allows code to raise the current label to
-- any value @newLabel@ such that @oldLabel ``canFlowTo`` newLabel &&
-- newLabel ``canFlowTo`` clearance@.
setLabelP :: PrivDesc l p => Priv p -> l -> LIO l ()
setLabelP p l = do
  guardAllocP p l `catch`
      \(_ :: MonitorFailure) -> throwLIO InsufficientPrivs
  modifyLIOStateTCB $ \s -> s { lioLabel = l }

-- | Returns the current value of the thread's clearance.
getClearance :: Label l => LIO l l
getClearance = lioClearance `liftM` getLIOStateTCB

-- | Lower the current clearance. The new clerance must be between
-- the current label and clerance. One cannot raise the current label
-- or create object with labels higher than the current clearance.
setClearance :: Label l => l -> LIO l ()
setClearance = setClearanceP noPrivs

-- | Raise the current clearance (undoing the effects of
-- 'setClearance') by exercising privileges. If the current label is
-- @l@ and current clearance is @c@, then @setClearanceP p cnew@
-- succeeds only if the new clearance is can flow to the current
-- clearance (modulo privileges), i.e., @'canFlowToP' p cnew c@ must
-- hold. Additionally, the current label must flow to the new
-- clearance, i.e., @l ``canFlowTo`` cnew@ must hold.
setClearanceP :: PrivDesc l p => Priv p -> l -> LIO l ()
setClearanceP p cnew = do
  LIOState l c <- getLIOStateTCB
  unless (canFlowToP p cnew c) $! throwLIO InsufficientPrivs
  unless (l `canFlowTo` cnew)  $! throwLIO CurrentLabelViolation
  putLIOStateTCB $ LIOState l cnew

-- | Runs an 'LIO' action and re-sets the current clearance to its
-- previous value once the action returns.  In particular, if the
-- action lowers the current clearance, the clearance will be restored
-- upon return.
--
-- Note that @scopeClearance@ always restores the clearance.  If
-- that causes the clearance to drop below the current label, a
-- 'ClearanceViolation' exception is thrown.  That exception can only
-- be caught outside a second @scopeClearance@ that restores the
-- clearance to higher than the current label.
scopeClearance :: Label l => LIO l a -> LIO l a
scopeClearance (LIOTCB action) = LIOTCB $ \sp -> do
  LIOState _ c <- readIORef sp
  ea <- IO.try $ action sp
  LIOState l _ <- readIORef sp
  writeIORef sp (LIOState l c)
  if l `canFlowTo` c
    then either (IO.throwIO :: SomeException -> IO a) return ea
    else IO.throwIO ClearanceViolation

-- | Lowers the clearance of a computation, then restores the
-- clearance to its previous value (actually, to the upper bound of
-- the current label and previous value).  Useful to wrap around a
-- computation if you want to be sure you can catch exceptions thrown
-- by it. The supplied clearance label must be bounded by the current
-- label and clearance as enforced by 'guardAlloc'.
-- 
-- Note that if the computation inside @withClearance@ acquires any
-- 'Priv's, it may still be able to raise its clearance above the
-- supplied argument using 'setClearanceP'.
withClearance :: Label l => l -> LIO l a -> LIO l a
withClearance c lio = scopeClearance $ setClearance c >> lio

-- | Same as 'withClearance', but uses privileges when applying
-- 'guardAllocP' to the supplied label.
withClearanceP :: PrivDesc l p => Priv p -> l -> LIO l a -> LIO l a
withClearanceP p c lio = scopeClearance $ setClearanceP p c >> lio


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
                    | ResultExceedsLabel
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
guardAlloc :: Label l => l -> LIO l ()
guardAlloc = guardAllocP noPrivs

-- | Like 'guardAlloc', but takes privilege argument to be more
-- permissive.  Note: privileges are /only/ used when checking that
-- the current label can flow to the given label.
guardAllocP :: PrivDesc l p => Priv p -> l -> LIO l ()
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
taint :: Label l => l -> LIO l ()
taint = taintP noPrivs

-- | Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that @taintP@ will never lower the current label.
-- It simply uses privileges to avoid raising the label as high as
-- 'taint' would raise it.
taintP :: PrivDesc l p => Priv p -> l -> LIO l ()
taintP p newl = do
  c <- getClearance
  l <- getLabel
  let l' = partDowngradeP p newl l
  unless (l' `canFlowTo` c) $! throwLIO ClearanceViolation
  modifyLIOStateTCB $ \s -> s { lioLabel = l' }


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
guardWrite = guardWriteP noPrivs

-- | Like 'guardWrite', but takes privilege argument to be more
-- permissive.
guardWriteP :: PrivDesc l p => Priv p -> l -> LIO l ()
guardWriteP p newl = do
  taintP      p newl
  guardAllocP p newl

--
-- Monad base
--

-- | Synonym for monad in which 'LIO' is the base monad.
class (Monad m, Label l) => MonadLIO l m | m -> l where
  -- | Lift an 'LIO' computation.
  liftLIO :: LIO l a -> m a

instance Label l => MonadLIO l (LIO l) where
  liftLIO = id

