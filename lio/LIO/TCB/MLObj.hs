{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

-- | IO Objects with mutable labels.  These are tricky to use
-- correctly--remember that a label itself can leak data--but are also
-- useful in some cases.
module LIO.TCB.MLObj where

import safe Control.Concurrent.MVar
import safe Control.Monad
import safe Data.Typeable

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label
import LIO.TCB

data MLObj label object = MLObjTCB
                          !(MVar label) -- ^ Mutable label
                          !label        -- ^ Object clearance
                          !object       -- ^ IO object
                          deriving (Typeable)

mlObjTCB :: (Label l) => l -> l -> a -> LIO l (MLObj l a)
mlObjTCB l c a = do
  unless (l `canFlowTo` c) $ labelError "mlObjTCB" [l, c]
  mv <- ioTCB $ newMVar l
  return $ MLObjTCB mv c a

clearanceOfMLObj :: MLObj l a -> l
clearanceOfMLObj (MLObjTCB _ c _) = c

-- | Return the label of an 'MLObj' if the label can be read with the
-- supplied privileges.  Otherwise return 'Nothing'.
--
-- This function is kind of tricky.  Either you can read the label and
-- know what it is, or you shouldn't find out anything about it.  We
-- can't call 'taintP' to raise the current label, as this would leak
-- whether the object's label is above the calling thread's clearance
-- (which could potentially encode secret information).
labelOfMLObjP :: (Label l, PrivDesc l p) =>
                 Priv p -> MLObj l a -> LIO l (Maybe l)
labelOfMLObjP p (MLObjTCB mv _ _) = do
  curl <- getLabel
  ioTCB $ withMVar mv $ \l ->
    return $ if canFlowToP p l curl then Just l else Nothing


modifyMLObjLabelTCB :: (Label l) => (l -> LIO l l) -> MLObj l a -> LIO l ()
modifyMLObjLabelTCB modifyLabel (MLObjTCB mv c _) =
  LIOTCB $ \s -> modifyMVar_ mv $ \lold -> do
    let LIOTCB io = do
          lnew <- modifyLabel lold
          unless (lnew `canFlowTo` c) $
            labelError "modifyMLObjLabelTCB" [lnew, c]
          return lnew
    io s

-- | Suitable function argument to 'modifyMLObjLabelTCB' for external
-- resources such as sockets, where you have to have sufficient
-- privileges to move between the old and new labels, because the
-- other end of the socket might do that.
externalMLObjPolicy :: (Label l, PrivDesc l p) => Priv p -> l -> l -> LIO l l
externalMLObjPolicy p lnew lold = do
  lcur <- getLabel
  ccur <- getClearance
  unless (canFlowToP p lcur lold && canFlowTo lnew ccur
          && canFlowToP p lold lnew && canFlowToP p lnew lold) $
    labelError "externalMLObjPolicy" [lold, lnew]
  guardWriteP p lold
  return lnew

-- | Suitable function argument to 'modifyMLObjLabelTCB' for internal
-- resources such as mutable variables that cannot be directly
-- accessed outside of 'LIO'.
internalMLObjPolicy :: (Label l, PrivDesc l p) => Priv p -> l -> l -> LIO l l
internalMLObjPolicy p lnew lold = do
  lcur <- getLabel
  ccur <- getClearance
  unless (canFlowToP p lcur lold && canFlowTo lnew ccur
          && canFlowToP p lold lnew) $
    labelError "internalMLObjPolicy" [lold, lnew]
  guardWriteP p lold
  return lnew

useMLObjTCB :: (Label l) =>
               (l -> LIO l ())  -- ^ Check label
            -> IO r             -- ^ Perform IO action
            -> MLObj l a
            -> LIO l r
useMLObjTCB check action (MLObjTCB mv _ _) =
  LIOTCB $ \s -> withMVar mv $ \lold -> do
    () <- case check lold of LIOTCB io -> io s
    action

withMLObjTCB :: (Label l) =>
               (l -> LIO l ())  -- ^ Check label
            -> (a -> IO r)      -- ^ Perform IO action on object
            -> MLObj l a
            -> LIO l r
withMLObjTCB check action mlo@(MLObjTCB _ _ a) =
  useMLObjTCB check (action a) mlo


#if 0
genTypesVals :: Int -> IO ()
genTypesVals n0 =
  putStrLn $ "#define TypesVals(macro) \\\n" ++ concatMap doit [1..n0]
  where doit n = "  macro(" ++ icl " -> " (as n) ++ ", \\\n" ++
                 "        " ++ icl " " (as n) ++ ")" ++
                 (if n < n0 then "; \\\n" else "\n")
        as n = map (\i -> "a" ++ show i) [1..n]
        icl _ [] = []
        icl s (h:t) = h ++ concatMap (\a -> s ++ a) t
#endif

#define TypesVals(macro) \
  macro(a1, \
        a1); \
  macro(a1 -> a2, \
        a1 a2); \
  macro(a1 -> a2 -> a3, \
        a1 a2 a3); \
  macro(a1 -> a2 -> a3 -> a4, \
        a1 a2 a3 a4); \
  macro(a1 -> a2 -> a3 -> a4 -> a5, \
        a1 a2 a3 a4 a5); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6, \
        a1 a2 a3 a4 a5 a6); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7, \
        a1 a2 a3 a4 a5 a6 a7); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8, \
        a1 a2 a3 a4 a5 a6 a7 a8); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9, \
        a1 a2 a3 a4 a5 a6 a7 a8 a9); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10, \
        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

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

mblessTCB :: (WrapIO l io lio, Label l) =>
             String -> (a -> io) -> MLObj l a -> lio
mblessTCB name io mlo@(MLObjTCB _ _ a) = wrapIO check (io a)
  where check ioa = withContext name $ useMLObjTCB guardWrite ioa mlo

mblessPTCB :: (WrapIO l io lio, Label l, PrivDesc l p) =>
             String -> (a -> io) -> Priv p -> MLObj l a -> lio
mblessPTCB name io p mlo@(MLObjTCB _ _ a) = wrapIO check (io a)
  where check ioa = withContext name $ useMLObjTCB (guardWriteP p) ioa mlo
