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
  , newMLabel, labelOfMlabel, readMLabelP, modifyMLabelP
  , newMLabelTCB, withMLabelTCB
  , MLabelOf(..)
  -- * 'MLabel' modificaton policies
  , MLabelPolicy(..), InternalML(..), ExternalML(..)
  -- * Objects with mutable labels
  , MLObj(..), mlObjTCB, mblessTCB, mblessPTCB
  ) where

import safe Control.Concurrent.FairRWLock
import safe Control.Monad
import safe Data.IORef
import safe Data.Typeable

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label
import LIO.TCB

-- | Class of policies for when it is permissible to update an
-- 'MLabel'.
class MLabelPolicy policy l where
  mlabelPolicy :: (PrivDesc l p) => ml policy l -> p -> l -> l -> LIO l ()

-- | 'InternalML' is for objects contained entirely within Haskell,
-- such as a variable.  Raising the label can't cause data to leak.
data InternalML = InternalML deriving Show
instance MLabelPolicy InternalML l where
  mlabelPolicy _ p lold lnew =
    unless (canFlowToP p lold lnew) $ labelError "InternalML" [lold, lnew]

-- | 'ExternalML' is for objects that communicate to the outside
-- world, where extra privileges are required since once data gets
-- out, so as to vouch for the fact that the other end of a socket
-- won't arbitrarily downgrade data.
data ExternalML = ExternalML deriving Show
instance MLabelPolicy ExternalML l where
  mlabelPolicy _ p lold lnew =
    unless (canFlowToP p lold lnew && canFlowToP p lnew lold) $
    labelError "ExternalML" [lold, lnew]

-- | A mutable label.  Consists of a static label on the label, a lock
-- for access to the mutable label, and a mutable label.
data MLabel policy l = MLabelTCB !l !RWLock !(IORef l)

newMLabelTCB :: l -> l -> LIO l (MLabel policy l)
newMLabelTCB ll l = do
  lk <- ioTCB $ Control.Concurrent.FairRWLock.new
  r <- ioTCB $ newIORef l
  return $ MLabelTCB ll lk r

newMLabel :: (PrivDesc l p) => Priv p -> l -> l -> LIO l (MLabel policy l)
newMLabel p ll l = do
  guardAllocP p ll
  guardAllocP p l
  newMLabelTCB ll l

labelOfMlabel :: MLabel policy l -> l
labelOfMlabel (MLabelTCB ll _ _) = ll

readMLabelP :: (PrivDesc l p) => Priv p -> MLabel policy l -> LIO l l
readMLabelP p (MLabelTCB ll _ r) = do
  taintP p ll
  ioTCB $ readIORef r

withMLabelTCB :: (Label l) =>
                 MLabel policy l -> (l -> LIO l a) -> LIO l a
withMLabelTCB (MLabelTCB _ lk r) fn = do
  s <- LIOTCB return
  ioTCB $ withRead lk $ do
    l <- readIORef r
    case fn l of LIOTCB io -> io s

withMLabelP :: (Label l, PrivDesc l p) =>
               Priv p -> MLabel policy l -> (l -> LIO l a) -> LIO l a
withMLabelP p ml@(MLabelTCB ll _ _) fn = withContext "withMLabelP" $ do
  taintP p ll
  withMLabelTCB ml fn

modifyMLabelP :: (Label l, PrivDesc l p, MLabelPolicy policy l) =>
                 Priv p -> MLabel policy l -> (l -> LIO l l) -> LIO l ()
modifyMLabelP p ml@(MLabelTCB ll lk r) fn =
  withContext "modifyMLabelP" $ do
    guardWriteP p ll
    s <- LIOTCB return
    let go = do
          lold <- ioTCB $ readIORef r
          lnew <- fn lold
          mlabelPolicy ml p lold lnew
          ioTCB $ writeIORef r lnew
    ioTCB $ withWrite lk $ case go of LIOTCB io -> io s


-- | Class of objects with mutable labels.
class MLabelOf t where
  mLabelOf :: t policy l a -> MLabel policy l


-- | IO Object with a mutable label.
data MLObj policy l object = MLObjTCB !(MLabel policy l) !object
                             deriving (Typeable)

instance MLabelOf MLObj where
  mLabelOf (MLObjTCB ml _) = ml

mlObjTCB :: (Label l) => l -> l -> a -> LIO l (MLObj policy l a)
mlObjTCB ll l a = do
  ml <- newMLabelTCB ll l
  return $ MLObjTCB ml a

#include "TypeVals.hs"

class WrapIO l io lio | l io -> lio where
  wrapIO :: (forall r. IO r -> LIO l r) -> io -> lio
instance WrapIO l (IO r) (LIO l r) where
  {-# INLINE wrapIO #-}
  wrapIO f = f
#define WRAPIO(types, vals) \
instance WrapIO l (types -> IO r) (types -> LIO l r) where { \
  {-# INLINE wrapIO #-}; \
  wrapIO f io vals = f $ io vals; \
}
TypesVals (WRAPIO)

mblessTCB :: (WrapIO l io lio, Label l, PrivDesc l p) =>
             String -> (a -> io) -> MLObj policy l a -> lio
mblessTCB name io (MLObjTCB ml a) = wrapIO check (io a)
  where check ioa = withContext name $ withMLabelP noPrivs ml $ \l -> do
          guardWrite l
          ioTCB ioa

mblessPTCB :: (WrapIO l io lio, Label l, PrivDesc l p) =>
              String -> (a -> io) -> Priv p -> MLObj policy l a -> lio
mblessPTCB name io p (MLObjTCB ml a) = wrapIO check (io a)
  where check ioa = withContext name $ withMLabelP p ml $ \l -> do
          guardWriteP p l
          ioTCB ioa

