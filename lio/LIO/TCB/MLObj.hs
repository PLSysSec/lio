{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

-- | Helper routines for exposing @IO@ operations on objects with
-- mutable labels.  The mutable labels are implemented by type
-- 'MLabel', and have an immutable meta-label (or \"label label\")
-- protecting the mutable label.
--
-- It is reasonable to allow untrusted code to modify labels by
-- exporting a type-restricted version of 'modifyMLObjLabelP'.  When
-- this happens, asynchronous exceptions are sent to any other threads
-- inside 'mblessTCB' or 'mblessPTCB' if the new label revokes their
-- access.
module LIO.TCB.MLObj (
  -- * Objects with mutable labels
    MLObj(..), mlObjTCB, mlPolicyObjTCB, modifyMLObjLabelP
  , mblessTCB, mblessPTCB
  -- * Internal details
  -- ** Mutable labels
  , MLabel(..)
  , newMLabelP, labelOfMlabel, readMLabelP, withMLabelP, modifyMLabelP
  , MLabelOf(..)
  -- ** MLabel modificaton policies
  , MLabelPolicyDefault(..), MLabelPolicy(..), InternalML(..), ExternalML(..)
  -- ** Helper class for variadic lifting
  , LabelIO(..)
  ) where

import safe Control.Concurrent
import safe qualified Control.Exception as IO
import safe Control.Monad
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.IORef
import safe Data.Typeable
import safe Data.Unique

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label
import LIO.TCB



-- | Class of policies for when it is permissible to update an
-- 'MLabel'.
class MLabelPolicy policy l where
  mlabelPolicy :: (PrivDesc l p) => policy -> p -> l -> l -> LIO l ()

-- | Class for 'MLabelPolicy's that don't encode any interesting
-- values.  This allows 'mlObjTCB' to create an 'MLObj' without
-- requiring a policy argument.
class MLabelPolicyDefault policy where
  mlabelPolicyDefault :: policy

-- | 'InternalML' is for objects contained entirely within Haskell,
-- such as a variable.  Raising the label can't cause data to leak.
data InternalML = InternalML deriving (Show, Typeable)
instance MLabelPolicy InternalML l where
  mlabelPolicy _ p lold lnew =
    unless (canFlowToP p lold lnew) $ labelError "InternalML" [lold, lnew]
instance MLabelPolicyDefault InternalML where
  mlabelPolicyDefault = InternalML

-- | 'ExternalML' is for objects that communicate to the outside
-- world, where extra privileges are required since once data gets
-- out, so as to vouch for the fact that the other end of a socket
-- won't arbitrarily downgrade data.
data ExternalML = ExternalML deriving (Show, Typeable)
instance MLabelPolicy ExternalML l where
  mlabelPolicy _ p lold lnew =
    unless (canFlowToP p lold lnew && canFlowToP p lnew lold) $
    labelError "ExternalML" [lold, lnew]
instance MLabelPolicyDefault ExternalML where
  mlabelPolicyDefault = ExternalML

-- | A mutable label.  Consists of a static label on the label, a
-- mutable label, and a list of threads currently accessing the label.
-- This is intended to be used by privileged code implementing @IO@
-- abstractions with mutable labels.  Routines for accessing such an
-- @IO@ abstraction should perform tne @IO@ from within a call to
-- 'withMLabelP', to ensure an exception is raised if another thread
-- revokes access with 'modifyMLabelP'.
data MLabel policy l = MLabelTCB {
    mlLabelLabel :: !l
  , mlLabel :: !(IORef l)
  , mlUsers :: !(MVar (Map Unique (l -> IO Bool)))
  , mlPolicy :: policy
  } deriving (Typeable)

-- | Returns the immutable label that controls access to the mutable
-- label value of an 'MLabel'.
labelOfMlabel :: MLabel policy l -> l
labelOfMlabel (MLabelTCB ll _ _ _) = ll

-- | Retreive a snapshot of the value of a mutable label.  Of course,
-- it may already have changed by the time you process it.
readMLabelP :: (PrivDesc l p) => Priv p -> MLabel policy l -> LIO l l
readMLabelP p (MLabelTCB ll r _ _) = do
  taintP p ll
  ioTCB $ readIORef r

-- | Run an action that should be protected by a mutable label.  An
-- exception is thrown if the invoking thread cannot write to the
-- mutable label given the privileges.  No attempt is made to adjust
-- the current label, even if doing so would make the permissions
-- acceptable.
--
-- Note that if the label changes after this function has been
-- invoked, an exception may be raised in the middle of the protected
-- action.
withMLabelP :: (PrivDesc l p) =>
               Priv p -> MLabel policy l -> LIO l a -> LIO l a
withMLabelP p (MLabelTCB ll r mv _) action = LIOTCB $ \s -> do
  let run (LIOTCB io) = io s
  run $ taintP p ll
  tid <- myThreadId
  u <- newUnique
  let check lnew = do
        LIOState { lioLabel = lcur, lioClearance = ccur } <- readIORef s
        if canFlowToP p lcur lnew && canFlowToP p lnew lcur
          then return True
          else do IO.throwTo tid LabelError {
                      lerrContext = []
                    , lerrFailure = "withMLabelP label changed"
                    , lerrCurLabel = lcur
                    , lerrCurClearance = ccur
                    , lerrPrivs = [GenericPrivDesc $ privDesc p]
                    , lerrLabels = [lnew]
                    }
                  return False
      enter = modifyMVar_ mv $ \m -> do
        void $ readIORef r >>= check
        return $ Map.insert u check m
      exit = modifyMVar_ mv $ return . Map.delete u
  IO.bracket_ enter exit $ run action

-- | Change the mutable label in an 'MLabel'.  Raises asynchronous
-- exceptions in other threads that are inside 'withMLabelP' if the
-- new label revokes their access.
modifyMLabelP :: (PrivDesc l p, MLabelPolicy policy l) =>
                 Priv p -> MLabel policy l -> (l -> LIO l l) -> LIO l ()
modifyMLabelP p (MLabelTCB ll r mv pl) fn = withContext "modifyMLabelP" $ do
  guardWriteP p ll
  s <- LIOTCB return
  let run (LIOTCB io) = io s
  ioTCB $ modifyMVar_ mv $ \m -> do
    lold <- readIORef r
    lnew <- run $ fn lold
    () <- run $ mlabelPolicy pl p lold lnew
    writeIORef r lnew
    Map.fromList `fmap` filterM (($ lnew) . snd) (Map.assocs m)

-- | @newMLabelP policy ll l@ creates an 'MLabel'.  @policy@ is a
-- policy specifying under what conditions it is permissible to change
-- the label.  @ll@ is the immutable label of the mutable label.  @l@
-- is the initial value of this mutable label.
newMLabelTCB :: policy -> l -> l -> LIO l (MLabel policy l)
newMLabelTCB policy ll l = do
  r <- ioTCB $ newIORef l
  mv <- ioTCB $ newMVar Map.empty
  return $ MLabelTCB ll r mv policy

-- | Create an 'MLabel', performing access control checks to ensure
-- that the labels are within the range allowed given the current
-- label and clearance, and the supplied privileges.
newMLabelP :: (PrivDesc l p) =>
              Priv p -> policy -> l -> l -> LIO l (MLabel policy l)
newMLabelP p policy ll l = do
  guardAllocP p ll
  guardAllocP p l
  newMLabelTCB policy ll l

-- | Class of objects with mutable labels.
class MLabelOf t where
  mLabelOf :: t policy l a -> MLabel policy l

-- | IO Object with a mutable label.  By contrast with
-- 'LIO.TCB.LObj.LObj', the label on an 'MLObj' can change over time.
-- If this happens, the internal 'MLabel' ensures that threads
-- accessing the object receive an asynchronous exception.
data MLObj policy l object = MLObjTCB !(MLabel policy l) !object
                             deriving (Typeable)

instance MLabelOf MLObj where
  mLabelOf (MLObjTCB ml _) = ml

-- | Like 'mlObjTCB', but create an 'MLObj' with a particular policy
-- value.  Note that you don't need to use this for 'ExternalML' and
-- 'InternalML', as these don't have anything interesting in the
-- policy value, only the type matters.  This might be useful if, for
-- instance, you wished to design a new policy type that embeds a
-- clearance.
mlPolicyObjTCB :: policy -> l -> l -> a -> LIO l (MLObj policy l a)
mlPolicyObjTCB policy ll l a = do
  ml <- newMLabelTCB policy ll l
  return $ MLObjTCB ml a

-- | @'mlObjTCB' ll l a@ creates an 'MLObj' wrapping some @IO@ object
-- @a@.  Here @ll@ is the label on the label, which remains immutable
-- over the lifetime of the 'MLObj'.  @l@ is the initial value of the
-- mutable lable.
mlObjTCB :: (MLabelPolicyDefault policy) =>
            l -> l -> a -> LIO l (MLObj policy l a)
mlObjTCB ll l a = do
  ml <- newMLabelTCB mlabelPolicyDefault ll l
  return $ MLObjTCB ml a

-- | Modify the 'MLabel' within an 'MLObj'.
modifyMLObjLabelP :: (PrivDesc l p, MLabelPolicy policy l) =>
                     Priv p -> MLObj policy l a -> (l -> LIO l l) -> LIO l ()
modifyMLObjLabelP p (MLObjTCB ml _) = modifyMLabelP p ml

-- | Takes a @'liftIO'@-like function and an @IO@ function of an
-- arbitrary number of arguments (up to 10).  Applies the arguments to
-- the @IO@ function, then passed the result to its argument funciton
-- to transform it into an @LIO@ function.
class LabelIO l io lio | l io -> lio where
  labelIO :: (forall r. IO r -> LIO l r) -> io -> lio
instance LabelIO l (IO r) (LIO l r) where
  {-# INLINE labelIO #-}
  labelIO f = f
#define WRAPIO(types, vals) \
instance LabelIO l (types -> IO r) (types -> LIO l r) where { \
  {-# INLINE labelIO #-}; \
  labelIO f io vals = f $ io vals; \
}

WRAPIO(a1, \
      a1); \
WRAPIO(a1 -> a2, \
      a1 a2); \
WRAPIO(a1 -> a2 -> a3, \
      a1 a2 a3); \
WRAPIO(a1 -> a2 -> a3 -> a4, \
      a1 a2 a3 a4); \
WRAPIO(a1 -> a2 -> a3 -> a4 -> a5, \
      a1 a2 a3 a4 a5); \
WRAPIO(a1 -> a2 -> a3 -> a4 -> a5 -> a6, \
      a1 a2 a3 a4 a5 a6); \
WRAPIO(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7, \
      a1 a2 a3 a4 a5 a6 a7); \
WRAPIO(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8, \
      a1 a2 a3 a4 a5 a6 a7 a8); \
WRAPIO(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9, \
      a1 a2 a3 a4 a5 a6 a7 a8 a9); \
WRAPIO(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10, \
      a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

-- | The 'MLObj' equivalent of 'blessTCB' in
-- "LIO.TCB.LObj#v:blessTCB".  Use this for conveniently providing
-- @LIO@ versions of standard @IO@ functions.
mblessTCB :: (LabelIO l io lio, Label l) =>
             String -> (a -> io) -> MLObj policy l a -> lio
{-# INLINE mblessTCB #-}
mblessTCB name io = mblessPTCB name io noPrivs

-- | The 'MLObj' equivalent of 'blessPTCB' in
-- "LIO.TCB.LObj#v:blessPTCB".  Use this for conveniently providing
-- @LIO@ versions of standard @IO@ functions.
mblessPTCB :: (LabelIO l io lio, Label l, PrivDesc l p) =>
              String -> (a -> io) -> Priv p -> MLObj policy l a -> lio
{-# INLINE mblessPTCB #-}
mblessPTCB name io p (MLObjTCB ml a) = labelIO check (io a)
  where check ioa = withContext name $ withMLabelP p ml $ ioTCB ioa
