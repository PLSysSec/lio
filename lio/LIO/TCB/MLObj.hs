{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

-- | IO Objects with mutable labels.  The mutable labels are
-- implemented by type 'MLabel', and have a static label protecting
-- them.
module LIO.TCB.MLObj (
  -- * Mutable labels
    MLabel(..)
  , newMLabelP, labelOfMlabel, readMLabelP, withMLabelP, modifyMLabelP
  , MLabelOf(..)
  -- * 'MLabel' modificaton policies
  , MLabelPolicyDefault(..), MLabelPolicy(..), InternalML(..), ExternalML(..)
  -- * Objects with mutable labels
  , MLObj(..), mlObjTCB, mlPolicyObjTCB, mblessTCB, mblessPTCB
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

-- | A mutable label.  Consists of a static label on the label, a lock
-- for access to the mutable label, and a mutable label.
data MLabel policy l = MLabelTCB {
    mlLabelLabel :: !l
  , mlLabel :: !(IORef l)
  , mlUsers :: !(MVar (Map Unique (l -> IO Bool)))
  , mlPolicy :: !policy
  } deriving (Typeable)

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

newMLabelTCB :: policy -> l -> l -> LIO l (MLabel policy l)
newMLabelTCB policy ll l = do
  r <- ioTCB $ newIORef l
  mv <- ioTCB $ newMVar Map.empty
  return $ MLabelTCB ll r mv policy

newMLabelP :: (PrivDesc l p) =>
              Priv p -> policy -> l -> l -> LIO l (MLabel policy l)
newMLabelP p policy ll l = do
  guardAllocP p ll
  guardAllocP p l
  newMLabelTCB policy ll l

-- | Class of objects with mutable labels.
class MLabelOf t where
  mLabelOf :: t policy l a -> MLabel policy l

-- | IO Object with a mutable label.
data MLObj policy l object = MLObjTCB !(MLabel policy l) !object
                             deriving (Typeable)

instance MLabelOf MLObj where
  mLabelOf (MLObjTCB ml _) = ml

mlPolicyObjTCB :: policy -> l -> l -> a -> LIO l (MLObj policy l a)
mlPolicyObjTCB policy ll l a = do
  ml <- newMLabelTCB policy ll l
  return $ MLObjTCB ml a

mlObjTCB :: (MLabelPolicyDefault policy) =>
            l -> l -> a -> LIO l (MLObj policy l a)
mlObjTCB ll l a = do
  ml <- newMLabelTCB mlabelPolicyDefault ll l
  return $ MLObjTCB ml a

#include "TypeVals.hs"

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
TypesVals (WRAPIO)

mblessTCB :: (LabelIO l io lio, Label l) =>
             String -> (a -> io) -> MLObj policy l a -> lio
{-# INLINE mblessTCB #-}
mblessTCB name io = mblessPTCB name io noPrivs

mblessPTCB :: (LabelIO l io lio, Label l, PrivDesc l p) =>
              String -> (a -> io) -> Priv p -> MLObj policy l a -> lio
{-# INLINE mblessPTCB #-}
mblessPTCB name io p (MLObjTCB ml a) = labelIO check (io a)
  where check ioa = withContext name $ withMLabelP p ml $ ioTCB ioa
