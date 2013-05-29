{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP,
             ConstraintKinds,
             FlexibleContexts #-}

{- |

A data type 'Labeled' protects access to pure values (hence, we refer
to values of type @'Label' a@ as /labeled values/).  The role of
labeled values is to allow users to associate heterogeneous labels (see
"LIO.Label") with values. Although LIO\'s current label protects all
values in scope with the current label, 'Labeled' values allow for
more fine grained protection. Moreover, trusted code may easily
inspect 'Labeled' values, for instance, when inserting values into a
database.

Without the appropriate privileges, one cannot produce a pure
/unlabeled/ value that depends on a secret 'Labeled' value, or
conversely produce a high-integrity 'Labeled' value based on pure
data.  This module exports functions for creating labeled values
('label'), using the values protected by 'Labeled' by unlabeling them
('unlabel'), and changing the value of a labeled value without
inspection ('relabelLabeledP', 'taintLabeled', 'untaintLabeled').  A
'Functor'-like class ('LabeledFunctor') on 'Labeled' is also defined
in this module.

-}

module LIO.Labeled (
    Labeled
  -- * Label values
  , label, labelP
  -- * Unlabel values
  , unlabel, unlabelP
  -- * Relabel values
  , relabelLabeledP
  , taintLabeled, taintLabeledP , untaintLabeledP
  , lFmap
#ifdef TO_LABELED
  -- * Executing sensitive computation
  -- $toLabeled
  , toLabeled, toLabeledP
  , discard, discardP
#endif
  ) where

import           LIO.TCB
import           LIO.Label
import           LIO.Core
import           LIO.Privs
import           LIO.Labeled.TCB
import           Control.Monad

#ifdef TO_LABELED
import qualified Control.Exception as E
#endif

-- | Returns label of a 'Labeled' type.
instance LabelOf Labeled where
  labelOf = labelOfLabeledTCB

--
-- Label values
--

-- | Function to construct a 'Labeled' from a label and pure value.  If
-- the current label is @lcurrent@ and the current clearance is
-- @ccurrent@, then the label @l@ specified must satisfy @lcurrent
-- ``canFlowTo`` l && l ``canFlowTo`` ccurrent@. Otherwise an
-- exception is thrown (see 'guardAlloc').
label :: MonadLIO l m => l -> a -> m (Labeled l a)
label = labelP NoPrivs

-- | Constructs a 'Labeled' using privilege to allow the `Labeled`'s
-- label to be below the current label.  If the current label is
-- @lcurrent@ and the current clearance is @ccurrent@, then the privilege
-- @p@ and label @l@ specified must satisfy @canFlowTo p lcurrent l@ and
-- @l ``canFlowTo`` ccurrent@.  Note that privilege is not used to bypass
-- the clearance.  You must use 'setClearanceP' to raise the clearance
-- first if you wish to create an 'Labeled' at a higher label than the
-- current clearance.
labelP :: (MonadLIO l m, Priv l p) => p -> l -> a -> m (Labeled l a)
labelP p l a = do
  guardAllocP p l
  return $! labelTCB l a

--
-- Unlabel values
--

-- | Within the 'LIO' monad, this function takes a 'Labeled' and returns
-- the underlying value.  Thus, in the 'LIO' monad one can say:
--
-- > x <- unlabel (xv :: Labeled SomeLabelType Int)
--
-- And now it is possible to use the value of @x :: Int@, which is the
-- pure value of what was stored in @xv@.  Of course, @unlabel@ also
-- raises the current label.  If raising the label would exceed the
-- current clearance, then @unlabel@ throws 'ClearanceViolation'.
-- However, you can use 'labelOf' to check if 'unlabel' will succeed
-- without throwing an exception.
unlabel :: MonadLIO l m => Labeled l a -> m a
unlabel = unlabelP NoPrivs

-- | Extracts the value of an 'Labeled' just like 'unlabel', but takes a
-- privilege argument to minimize the amount the current label must be
-- raised.  Function will throw 'ClearanceViolation' under the same
-- circumstances as 'unlabel'.
unlabelP :: (MonadLIO l m, Priv l p) => p -> Labeled l a -> m a
unlabelP p lv = do
  taintP p $! labelOf lv
  return $! unlabelTCB lv

--
-- Relabel values
--

-- | Relabels a 'Labeled' value to the supplied label if the given
-- privilege privileges permits it. It must be that the original
-- label and new label are equal, modulo the supplied privileges. In
-- other words the label remains in the same congruence class.
--
-- Consequently @relabelP p l lv@ throws an 'InsufficientPrivs'
-- exception if
--
-- @'canFlowToP' p l ('labelOf' lv) && 'canFlowToP' p ('labelOf' lv) l@
--
-- does not hold.
relabelLabeledP :: (MonadLIO l m, Priv l p)
                => p -> l -> Labeled l a -> m (Labeled l a)
relabelLabeledP p newl lv = do
  let origl = labelOf lv
  unless (canFlowToP p newl origl &&
          canFlowToP p origl newl) $ throwLIO InsufficientPrivs
  return . labelTCB newl $! unlabelTCB lv

-- | Raises the label of a 'Labeled' to the 'upperBound' of it's current
-- label and the value supplied.  The label supplied must be bounded by
-- the current label and clearance, though the resulting label may not be
-- if the 'Labeled' is already above the current thread's clearance. If
-- the supplied label is not bounded then @taintLabeled@ will throw an
-- exception (see 'guardAlloc').
taintLabeled :: MonadLIO l m => l -> Labeled l a -> m (Labeled l a)
taintLabeled = taintLabeledP NoPrivs

-- | Same as 'taintLabeled', but uses privileges when comparing the
-- current label to the supplied label. In other words, this function
-- can be used to lower the label of the labeled value by leveraging
-- the supplied privileges.
taintLabeledP :: (MonadLIO l m, Priv l p)
              => p -> l -> Labeled l a -> m (Labeled l a)
taintLabeledP p l lv = do
  guardAllocP p l
  return . labelTCB (l `upperBound` labelOf lv) $! unlabelTCB lv

-- | Same as 'untaintLabeled' but uses the supplied privileges when
-- downgrading the label of the labeled value.
untaintLabeledP :: (MonadLIO l m, Priv l p)
                => p -> l -> Labeled l a -> m (Labeled l a)
untaintLabeledP p target lv =
  relabelLabeledP p (partDowngradeP p (labelOf lv) target) lv


{- $functor

Making 'Labeled' an instance of 'Functor' is problematic because:

1. 'fmap' would have type @Labeled l a -> (a -> b) -> Labeled b@ and thus 
    creating /new/ labeled values above the current clearance or below
    the current label would be feasible (given one such value).
2. 'LIO' is polymorphic in the label type and thus 'fmap' would is
   susceptible to /refinement attacks/. Superficially if the label type
   contains an integrity component (see for example "LIO.DCLabel")
   then @fmap (\ -> 3) lv@ would produce a high-integrity labeled @3@
   if @lv@ is a high-integrity labeled value without any any authority
   or /endorsement/.

As a result, we provide a class 'LabeledFunctor' that export 'lFmap'
(labeled 'lFmap') that addressed the above issues. Firstly, each newly
created value is in the 'LIO' monad and secondly each label format
implementation must produce their own definition of 'lFmap' such that
the end label protects the computation result accordingly.

-}

-- | TODO(alevy): fix docs
-- IFC-aware functor instance. Since certain label formats may contain
-- integrity information, this is provided as a class rather than a
-- function. Such label formats will likely wish to drop endorsements in
-- the new labeled valued.
lFmap :: MonadLIO l m => Labeled l a -> (a -> b) -> m (Labeled l b)
lFmap lv f = do
  lc <- getLabel
  -- Result label is joined with current label
  let lres = labelOf lv `lub` lc
  -- `label` checks for clearance violation then labels
  label lres $ f (unlabelTCB lv)


#ifdef TO_LABELED

{- $toLabeled

LIO provides a means for executing a sensitive computation without
raising the current label. This is done with the function 'toLabeled'.
The semantics of the function is as follows:

1. Execute whatever action is supplied. This action may raise the
current label or lower the current clearance.

2. Take the result of the computation and wrap it in a 'Labeled'
value. The label of the value is provided as an argument to
'toLabeled'. Hence, the computation should not observe anything more
sensitive than this.

3. Restore the current label and clearance to that of step 1. Return
the 'Labeled' value created in step 2, if the end current label is not
above the value's label. Otherwise raise an exception to indicate that
the computation read data more sensitive than the set bound.

Note that if the executed computation raises an exception, 'toLabeled'
hides the exception -- this exception is only propagated when the
'Labeled' value is 'unlabel'ed.

NOTE:
As indicated by the warnings, 'toLabeled' is not safe with respect to
/termination sensitive non-interference/. This means that a malicious
computation can carry out an attack that leaks information by not
terminating in a 'toLabeled' block. Similarly an attacker can leverage
timing to do the same.  Your version of LIO has been compiled with the
@toLabeled@ flag, and thus susceptible to such attacks. If this was
not by choice, recompile the package (by default, 'toLabeled' is not
included) and use the primitives in "LIO.Concurrent" to implement a
similarly-behaving program.

-}

-- | @toLabeled@ is the dual of 'unlabel'.  It allows one to invoke
-- computations that would raise the current label, but without
-- actually raising the label.  Instead, the result of the
-- computation is packaged into a 'Labeled' with a supplied
-- label. (Of couse, the computation executed by @toLabeled@ must
-- most observe any data whose label exceeds the supplied label.)
--
-- To get at the result of the computation one will have to call
-- 'unlabel' and raise the label, but this can be postponed, or
-- done inside some other call to 'toLabeled'.  This suggests that
-- the provided label must be above the current label and below
-- the current clearance.  --
-- Note that @toLabeled@ always restores the clearance to whatever
-- it was when it was invoked, regardless of what occurred in the
-- computation producing the value of the 'Labeled'.  This highlights
-- one main use of clearance: to ensure that a @Labeled@ computed
-- does not exceed a particular label.
--
-- If an exception is thrown within a @toLabeled@ block, such that
-- the outer context is withing a 'catchLIO', which is further within
-- a @toLabeled@ block, infromation can be leaked. Consider the
-- following program that uses 'DCLabel's. (Note that 'discard' is
-- simply @toLabeled@ that throws throws the result away.)
--
--  > main = evalDC' $ do
--  >   lRef <- newLIORef bottom ""
--  >   hRef <- newLIORef top "secret"
--  >   -- brute force:
--  >   forM_ ["fun", "secret"] $ \guess -> do
--  >     stash <- readLIORef lRef
--  >     writeLIORef lRef $ stash ++ "\n" ++ guess ++ ":"
--  >     discard top $ do
--  >       catchLIO ( discard top $ do
--  >                    secret <- readLIORef hRef
--  >                    when (secret == guess) $ throwIO . userError $ "got it!"
--  >                ) (\(e :: IOError) -> return ())
--  >       l <- getLabel
--  >       when (l == bottom) $ do stash <- readLIORef lRef
--  >                             writeLIORef lRef $ stash ++ "no!"
--  >   readLIORef lRef
--  >     where evalDC' act = do (r,l) <- runDC act
--  >                            putStrLn r
--  >                            putStrLn $ "label = " ++ prettyShow l
--
--  The output of the program is:
--
--  > $ ./new
--  >
--  > fun:no!
--  > secret:
--  > label = <True , False>
--
-- Note that the current label is 'bottom' (which in DCLabels is
-- @<True , False>@), and the secret is leaked. The fundamental issue
-- is that the outer 'discard' allows for the current label to remain
-- low even though the 'catchLIO' raised the current label when the
-- secret was found (and thus exception was throw). As a consequence,
-- 'toLabeled' catches all exceptions, and returns a 'Labeled'
-- value that may have a labeled exception as wrapped by @throw@.
-- All exceptions within the outer computation, including
-- IFC violation attempts, are essentially rethrown when performing
-- an 'unlabel'.
--
toLabeled :: Label l
          => l       -- ^ Label of result and upper bound on
                     --  inner-computations' observation
          -> LIO l a -- ^ Inner computation
          -> LIO l (Labeled l a)
toLabeled = toLabeledP NoPrivs
{-# WARNING toLabeled "toLabeled is susceptible to termination attacks" #-}

-- | Same as 'toLabeled' but allows one to supply a privilege object
-- when comparing the initial and final label of the computation.
--
toLabeledP :: Priv l p
           => p -> l -> LIO l a -> LIO l (Labeled l a)
toLabeledP p l act = do
  -- Check that the supplied upper bound is bounded
  guardAllocP p l
  -- Get current state:
  save_s <- getLIOStateTCB
  -- Execute action, catching any exceptions:
  res <- (liftM Right act) `catchTCB` (return . Left . lubErr l)
  -- Get the new state:
  s   <- getLIOStateTCB
  let lastL = lioLabel s
  -- Restore state
  putLIOStateTCB save_s
  return . labelTCB l $
    if canFlowToP p lastL l
           -- If the result was an exception, rethrow it when unlabel
           -- otherwise just return the result
      then either E.throw id res
           -- Violated the upper bound:
      else let l1  = lastL `upperBound` l
               -- Join of exception label (if any) and l1
               l2 = either (upperBound l1 . getErrLabel) (const l1) res
               -- Throw pure exception with label l1
           in E.throw $ LabeledExceptionTCB l2 $ E.toException $ 
                  VMonitorFailure { monitorFailure = CanFlowToViolation
                                  , monitorMessage = errMsg }
    where lubErr lnew (LabeledExceptionTCB le e) =
                  LabeledExceptionTCB (le `lub` lnew) e
          --
          getErrLabel (LabeledExceptionTCB le _) = le
          --
          errMsg = "Computation read data more sensitive than bound."
{-# WARNING toLabeledP "toLabeledP is susceptible to termination attacks" #-}


-- | Executes a computation that would raise the current label, but
-- discards the result so as to keep the label the same.  Used when
-- one only cares about the side effects of a computation.  For
-- instance, if @log_handle@ is an 'LHandle' with a high label, one
-- can execute
--
-- @
--   discard top $ 'hputStrLn' log_handle \"Log message\"
-- @
--
-- to create a log message without affecting the current label.
--
discard :: Label l => l -> LIO l a -> LIO l ()
discard = discardP NoPrivs
{-# WARNING discard "discard is susceptible to termination attacks" #-}

-- | Same as 'discard', but uses privileges when comparing initial and
-- final label of the computation.
discardP :: Priv l p => p -> l -> LIO l a -> LIO l ()
discardP p l act = void $ toLabeledP p l act
{-# WARNING discardP "discardP is susceptible to termination attacks" #-}
#endif
