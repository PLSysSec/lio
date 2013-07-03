{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{- |

This module exports exception types thrown in response to label
failures.  In addition, it provides 'withContext', a function that
annotates any exceptions in the 'AnyLabelError' hierarchy that are
thrown within a given scope.  These annotations should be used to add
function names to exceptions, so as to make it easier to pinpoint the
cause of a label error.

-}

module LIO.Error (
    Annotatable(..), withContext
  , AnyLabelError(..), lerrToException, lerrFromException
  , GenericPrivDesc(..), LabelError(..),  labelError, labelErrorP
  , InsufficientPrivs(..), insufficientPrivs
  , ResultExceedsLabel(..)
  ) where

import safe qualified Control.Exception as IO
import safe Data.Typeable

import safe LIO.Exception
import safe LIO.Label
import LIO.TCB

-- | Class of error messages that can be annotated with context.
class Annotatable e where
  annotate :: String -> e -> e

-- | Parent of all label-related exceptions.
data AnyLabelError = forall e. (Exception e, Annotatable e) =>
                     AnyLabelError e deriving Typeable

instance Show AnyLabelError where
  showsPrec d (AnyLabelError e) = showsPrec d e

instance Annotatable AnyLabelError where
  annotate s (AnyLabelError e) = AnyLabelError $ annotate s e

instance Exception AnyLabelError

-- | Executes an action with a context string, which will be added to
-- any label exception thrown.
-- 
-- Note: this function wraps an action with a 'catch', and thus may
-- incur a small runtime cost (though it is well under 100 ns on
-- machines we benchmarked).
withContext :: String -> LIO l a -> LIO l a
withContext ctx (LIOTCB act) =
  LIOTCB $ \st -> act st `IO.catch` \e ->
  IO.throwIO $ annotate ctx (e :: AnyLabelError)

-- | Definition of 'toException' for children of 'AnyLabelError' in
-- the exception hierarchy.
lerrToException :: (Exception e, Annotatable e) => e -> SomeException
lerrToException = toException . AnyLabelError

-- | Definition of 'fromException' for children of 'AnyLabelError' in
-- the exception hierarchy.
lerrFromException :: (Exception e) => SomeException -> Maybe e
lerrFromException se = do
  AnyLabelError e <- fromException se
  cast e


-- | A generic privilege description for recording relevant privileges
-- in exceptions.
data GenericPrivDesc l = forall p. (PrivDesc l p) => GenericPrivDesc p
instance Show (GenericPrivDesc l) where
  showsPrec d (GenericPrivDesc p) = showsPrec d p


-- | Main error type thrown by label failures in the 'LIO' monad.
data LabelError l = LabelError {
    lerrContext :: [String] -- ^ Annotation of where the failure happened.
  , lerrFailure :: String   -- ^ Actual function that failed.
  , lerrCurLabel :: l       -- ^ Current label at time of error.
  , lerrCurClearance :: l   -- ^ Current clearance at time of error.
  , lerrPrivs :: [GenericPrivDesc l] -- ^ Any privileges involved in error.
  , lerrLabels :: [l]       -- ^ Any labels involved in error.
  } deriving (Show, Typeable)

instance Annotatable (LabelError l) where
  annotate a e = e { lerrContext = a : lerrContext e }

instance Label l => Exception (LabelError l) where
  toException = lerrToException
  fromException = lerrFromException

-- | Throw a label-error exception.
labelError :: (Label l) => String -- ^ Function that failed.
                        -> [l]    -- ^ Labels involved in error.
                        -> LIO l a
labelError fl ls = do
  st <- getLIOStateTCB
  throwLIO LabelError {
      lerrContext = []
    , lerrFailure = fl
    , lerrCurLabel = lioLabel st
    , lerrCurClearance = lioClearance st
    , lerrPrivs = []
    , lerrLabels = ls
    }

-- | Throw a label-error exception.
labelErrorP :: (Label l, PrivDesc l p) => String  -- ^ Function that failed.
                                       -> Priv p  -- ^ Privileges involved.
                                       -> [l]     -- ^ Labels involved.
                                       -> LIO l a
labelErrorP fl p ls = do
  st <- getLIOStateTCB
  throwLIO LabelError {
      lerrContext = []
    , lerrFailure = fl
    , lerrCurLabel = lioLabel st
    , lerrCurClearance = lioClearance st
    , lerrPrivs = [GenericPrivDesc $ privDesc p]
    , lerrLabels = ls
    }


-- | Error indicating insufficient privileges (independent of the
-- current label).  This exception is thrown by 'delegate', and
-- should also be thrown by gates that receive insufficient privilege
-- descriptions (see "LIO.Delegate").
data InsufficientPrivs = forall p. (SpeaksFor p) => InsufficientPrivs {
    inspContext :: [String]
  , inspFailure :: String
  , inspSupplied :: p
  , inspNeeded :: p
  } deriving (Typeable)

instance Show InsufficientPrivs where
  showsPrec _ (InsufficientPrivs c l s n) =
    ("InsufficientPrivs { inspContext = " ++) . shows c .
    (", inspLocation = " ++) . shows l .
    (", inspSupplied = " ++) . shows s .
    (", inspNeeded = " ++) . shows n .
    (" }" ++)

instance Annotatable InsufficientPrivs where
  annotate a e = e { inspContext = a : inspContext e }

instance Exception InsufficientPrivs where
  toException = lerrToException
  fromException = lerrFromException

-- | Raise 'InsufficientPrivs' error.
insufficientPrivs :: (SpeaksFor p) =>
                     String     -- ^ Function in which error occurs
                     -> p       -- ^ Description of privileges supplied
                     -> p       -- ^ Description of privileges needed
                     -> a
insufficientPrivs fl supplied needed
  | isPriv supplied = error $ "insufficientPrivs: " ++ show fl ++
                      " supplied actual privileges instead of description"
  | otherwise = IO.throw $ InsufficientPrivs [] fl supplied needed

-- | Error raised when a computation spawned by 'lFork' terminates
-- with its current label above the label of the result.
data ResultExceedsLabel l = ResultExceedsLabel {
    relContext :: [String]
  , relLocation :: String
  , relDeclaredLabel :: l
  , relActualLabel :: Maybe l
  } deriving (Show, Typeable)

instance Annotatable (ResultExceedsLabel l) where
  annotate a e = e { relContext = a : relContext e }

instance (Label l) => Exception (ResultExceedsLabel l) where
  toException = lerrToException
  fromException = lerrFromException
