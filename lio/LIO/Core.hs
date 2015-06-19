{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 

This module implements the core of the Labeled IO (LIO) information
flow control (IFC) library.  It provides a monad, 'LIO', that is
intended to be used as a replacement for the 'IO' monad in untrusted
code.  The idea is for untrusted code to provide a computation in the
'LIO' monad, which trusted code can then safely execute through
'evalLIO' and similar functions (e.g., 'evalDC' in
"LIO.DCLabel#v:evalDC").

Unlike 'IO', the 'LIO' monad keeps track of a /current label/
(accessible via the 'getLabel' function) during each computation. The
current label effectively tracks the sensitivity of all the data that
the computation has observed.  For example, if the computation has
read a \"secret\" mutable reference (see "LIO.LIORef") and then the
result of a \"top-secret\" thread (see "LIO.Concurrent") then the
current label will be at least \"top-secret\".  Labels are described
in more detail in the documentation for "LIO.Label", as well as the
documentation for particular label formats (such as "LIO.DCLabel").

The role of the current label is two-fold:  First, the current label
protects all pure values currently in scope.  For example, the current
label is the label on constants (such as @3@ and @\"tis a string\"@)
as well as function arguments.  More interestingly, consider reading a
secret reference:

> val <- readLIORef secret

Though the label of @secret@ may be \"secret\", @val@ is not
explicitly labeled.  Hence, to protect the contents of the 'LIORef'
that has been read into @val@, the current label must be at least
\"secret\" before returning from @readLIORef@.  More generally, if the
current label is @l_cur@, then it is only permissible to read data
labeled @l_r@ if @l_r ``canFlowTo`` l_cur@.  Note that, instead of
throwing an exception, reading data often just increases the current
label to ensure that @l_r ``canFlowTo`` l_cur@.  This is acomplished
using a function such as 'taint'.

The second purpose of the current label is to prevent inforation leaks
into public channels. Specifically, it is only permissible to modify
or write to data labeled @l_w@ when @l_cur``canFlowTo`` l_w@. Thus,
the following attempt to leak the @val@ after reading it from a secret
'LIORef' would fail:

> writeLIORef public val

In addition to the current label, the LIO monad keeps a second label,
the /current clearance/ (accessible via the 'getClearance' function).
The clearance can be used to enforce a \"need-to-know\" policy, since
it represents the highest value the current label can be raised to.
In other words, if the current clearance is @l_clear@ then the
computation cannot create, read or write to objects labeled @l@ such
that @(l ``canFlowTo`` l_clear) == False@.

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
  , AnyLabelError(..), LabelError(..), InsufficientPrivs(..)
  -- * Guards
  -- $guards

  -- ** Allocate/write-only guards
  , guardAlloc, guardAllocP
  -- ** Read-only guards
  , taint, taintP
  -- ** Read-write guards
  , guardWrite, guardWriteP
  ) where


import safe qualified Control.Exception as IO
import safe Control.Monad
import safe Data.IORef

import safe LIO.Error
import safe LIO.Exception
import safe LIO.Label
import safe LIO.Monad(MonadLIO(..))
import safe LIO.Run
import LIO.TCB


--
-- Internal state
--

-- | Returns the value of the thread's current label.
getLabel :: Label l => LIO l l
getLabel = lioLabel `liftM` getLIOStateTCB


-- | Raises the current label to the provided label, which must be
-- between the current label and clearance. See 'taint'.
setLabel :: Label l => l -> LIO l ()
setLabel l = withContext "setLabel" $ do
  guardAlloc l
  modifyLIOStateTCB $ \s -> s { lioLabel = l }

-- | If the current label is @oldLabel@ and the current clearance is
-- @clearance@, this function allows code to raise the current label
-- to any value @newLabel@ such that @'canFlowToP' priv oldLabel
-- newLabel && 'canFlowTo' newLabel clearance@.  (Note the privilege
-- argument affects the label check, not the clearance check; call
-- 'setClearanceP' first to raise the clearance.)
setLabelP :: PrivDesc l p => Priv p -> l -> LIO l ()
setLabelP p l = withContext "setLabelP" $ do
  guardAllocP p l
  modifyLIOStateTCB $ \s -> s { lioLabel = l }

-- | Returns the thread's current clearance.
getClearance :: Label l => LIO l l
getClearance = lioClearance `liftM` getLIOStateTCB

-- | Lowers the current clearance.  The new clerance must be between
-- the current label and previous current clerance.  One cannot raise
-- the current label or create object with labels higher than the
-- current clearance.
setClearance :: Label l => l -> LIO l ()
setClearance cnew = do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowTo l cnew && canFlowTo cnew c) $
    labelError "setClearance" [cnew]
  putLIOStateTCB $ LIOState l cnew

-- | Raises the current clearance (undoing the effects of
-- 'setClearance') by exercising privileges.  If the current label is
-- @l@ and current clearance is @c@, then @setClearanceP p cnew@
-- succeeds only if the new clearance is can flow to the current
-- clearance (modulo privileges), i.e., @'canFlowToP' p cnew c ==
-- True@.  Additionally, the current label must flow to the new
-- clearance, i.e., @l ``canFlowTo`` cnew@ == True.
-- 
-- Since LIO guards that are used when reading/writing data (e.g.,
-- 'guardAllocP') do not use privileges when comparing labels with the
-- current clearance, code must always raise the current clearance, to
-- read/write data above the current clearance.
setClearanceP :: PrivDesc l p => Priv p -> l -> LIO l ()
setClearanceP p cnew = do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowTo l cnew && canFlowToP p cnew c) $
    labelErrorP "setClearanceP" p [cnew]
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
    else IO.throwIO LabelError { lerrContext = []
                               , lerrFailure = "scopeClearance"
                               , lerrCurLabel = l
                               , lerrCurClearance = c
                               , lerrPrivs = []
                               , lerrLabels = [] }

-- | Temporarily lowers the clearance for a computation, then restores
-- it.  Equivalent to:
--
-- @
-- withClearance c lio = 'scopeClearance' $ 'setClearance' c >> lio
-- @
-- 
-- Note that if the computation inside @withClearance@ acquires any
-- 'Priv's, it may still be able to raise its clearance above the
-- supplied argument using 'setClearanceP'.
withClearance :: Label l => l -> LIO l a -> LIO l a
withClearance c lio = scopeClearance $ setClearance c >> lio

-- | A variant of 'withClearance' that takes privileges.  Equivalent
-- to:
--
-- @
-- withClearanceP p c lio = 'scopeClearance' $ 'setClearanceP' p c >> lio
-- @
withClearanceP :: PrivDesc l p => Priv p -> l -> LIO l a -> LIO l a
withClearanceP p c lio = scopeClearance $ setClearanceP p c >> lio


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
     lcurrent@.  Specifically, it does this by computing the least
     upper bound or 'lub' of the two labels.  (Note 'taint' will fail
     if the new label cannot flow to the current clearance.)

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

Note that a write /always/ implies a read.  Hence, when writing to an
object for which you can observe the result (which is frequently the
case), you must use 'guardWrite'.  However, when performing a write
for which there are no observable side-effects to the writer, i.e.,
you cannot observe the success or failure of the write, then it is
safe to use 'guardAlloc'.

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
guardAlloc :: Label l => l -> LIO l ()
guardAlloc newl = do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowTo l newl && canFlowTo newl c) $
    labelError "guardAllocP" [newl]

-- | Like 'guardAlloc', but takes a privilege argument to be more
-- permissive.  Note: privileges are /only/ used when checking that
-- the current label can flow to the target label; @guardAllocP@ still
-- always throws an exception when the target label is higher than the
-- current clearance.
guardAllocP :: PrivDesc l p => Priv p -> l -> LIO l ()
guardAllocP p newl = do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  unless (canFlowToP p l newl && canFlowTo newl c) $
    labelErrorP "guardAllocP" p [newl]

--
-- Read
--

-- | Use @taint l@ in trusted code before observing an object labeled
-- @l@.  This will raise the current label to a value @l'@ such that
-- @l ``canFlowTo`` l'@, or throw a 'LabelError' exception if @l'@
-- would have to be higher than the current clearance.
taint :: Label l => l -> LIO l ()
taint newl = do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  let l' = l `lub` newl
  unless (l' `canFlowTo` c) $ labelError "taint" [newl]
  modifyLIOStateTCB $ \s -> s { lioLabel = l' }
  

-- | Like 'taint', but use privileges to reduce the amount of taint
-- required.  Note that @taintP@ will never lower the current label.
-- It simply uses privileges to avoid raising the label as high as
-- 'taint' would raise it.
taintP :: PrivDesc l p => Priv p -> l -> LIO l ()
taintP p newl = do
  LIOState { lioLabel = l, lioClearance = c } <- getLIOStateTCB
  let l' = l `lub` downgradeP p newl
  unless (l' `canFlowTo` c) $ labelErrorP "taintP" p [newl]
  modifyLIOStateTCB $ \s -> s { lioLabel = l' }


-- | Use @guardWrite l@ in any (trusted) code before modifying an
-- object labeled @l@, for which the modification can be observed,
-- i.e., the write implies a read.
--
-- The implementation of @guardWrite@ is straight forward:
--
-- > guardWrite l = guardAlloc l >> taint l
--
-- The 'guardAlloc' ensures that we can write(-only) to the object
-- labeled @l@, i.e., the current label ``canFlowTo`` @l@ (and @l@
-- ``canFlowTo`` the current clearance). If this check succeeds then
-- we raise the current label with 'taint' to reflect the fact that
-- this is a also a read effect. Note that if the write-only guard
-- succeeds, the 'taint' will always suceed (we're simply raising the
-- current label to a label that is below the clearance).
--
guardWrite :: Label l => l -> LIO l ()
guardWrite newl = withContext "guardWrite" $ do
  guardAlloc newl
  taint newl

-- | Like 'guardWrite', but takes a privilege argument to be more
-- permissive.
guardWriteP :: PrivDesc l p => Priv p -> l -> LIO l ()
guardWriteP p newl = withContext "guardWriteP" $ do
  guardAllocP p newl
  taintP p newl
