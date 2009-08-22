{-# OPTIONS_GHC -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving #-}

module LIO.TCB ( POrdering(..), POrd(..), o2po, Label(..)
               , Lref, taint, label, Priv(..)
               , LIO, getL, putL, discard
               , lputStr, lputStrLn
               -- Functions below are part of the TCB
               , TCB(..), evalLIO, lio, getLS, putLS
               ) where

import Control.Monad.State.Lazy hiding (put, get)
import Data.Monoid

--
-- A data type only accessible to trusted code in the trusted computing base
--

data TCB = TCB

--
-- We need a partial order and a Label
--

data POrdering = PEQ | PLT | PGT | PNE deriving (Eq, Ord, Show)

instance Monoid POrdering where
    mempty          = PEQ
    mappend PLT PGT = PNE
    mappend PGT PLT = PNE
    mappend x y     = max x y

class (Eq a) => POrd a where
    pcompare :: a -> a -> POrdering
    leq :: a -> a -> Bool

    pcompare a b | a == b = PEQ
                 | a `leq` b = PLT
                 | b `leq` a = PGT
                 | otherwise = PNE
    leq a b = case pcompare a b of
                PEQ       -> True
                PLT       -> True
                otherwise -> False

o2po EQ = PEQ; o2po LT = PLT; o2po GT = PGT
-- instance (Ord a) => POrd a where pcompare = o2po . compare

class (POrd a) => Label a where
    lpure :: a                  -- label for pure values
    lclear :: a                 -- default clearance
    lub :: a -> a -> a
    glb :: a -> a -> a


--
-- Labeled value - Lref
--

data (Label l) => Lref l t = Lref l t deriving (Show)

instance Label l => Functor (Lref l) where
    fmap f (Lref l t) = Lref (l `lub` lpure) (f t)

instance Label l => Monad (Lref l) where
    return x = Lref lpure x
    (Lref l x) >>= f = case f x of
                         Lref l' y -> Lref (lub l l') y

instance Label l => MonadFix (Lref l) where
    mfix f = fix g
        where
          g ~(Lref _ a) = f a

taint               :: (Label l) => l -> Lref l a -> Lref l a
taint l' (Lref l a) = Lref (lub l l') a

label       :: (Label l) => l -> t -> Lref l t
label l obj = Lref (lub l lpure) obj

relabel                    :: (Label l) => l -> (Lref l t) -> (Lref l t)
relabel newl (Lref oldl o) = if oldl `leq` newl
                             then (Lref newl o)
                             else error "relabel denied"

class (Label l, Monoid p) => Priv l p where
    lostar :: p -> l -> l
    histar :: p -> l -> l

    unlabel :: p -> (Lref l t) -> t
    unlabel p (Lref l o) = if lostar p l `leq` lpure
                           then o
                           else error "unlabel denied"

    downgrade :: p -> l -> (Lref l t) -> (Lref l t)
    downgrade p newl (Lref oldl o) = if lostar p oldl `leq` newl
                                     then (Lref newl o)
                                     else error "downgrade denied"


--
-- Labeled IO
--

data LIOstate l s = LIOstate { labelState :: s
                             , lioL :: l -- current label
                             , lioC :: l -- current clearance
                             }

newtype LIO l s a = LIO (StateT (LIOstate l s) IO a)
    deriving (Functor, Monad, MonadFix)

runLIO :: Label l => LIO l s a -> LIOstate l s -> IO (a, LIOstate l s)
runLIO (LIO (StateT f)) = f

mkLIO :: Label l => (LIOstate l s -> IO (a, LIOstate l s)) -> LIO l s a
mkLIO f = LIO (StateT f)

get :: Label l => LIO l s (LIOstate l s)
get = mkLIO $ \s -> return (s, s)

getLS :: Label l => LIO l s s
getLS = get >>= return . labelState

put :: Label l => LIOstate l s -> LIO l s ()
put s = mkLIO $ \_ -> return (() , s)

putLS :: Label l => s -> LIO l s ()
putLS ls = get >>= put . update
    where update s = s { labelState = ls }

newLIO :: Label l => s -> LIOstate l s
newLIO s = LIOstate { labelState = s
                    , lioL = lpure
                    , lioC = lclear
                    }

evalLIO :: (Label l) => s -> LIO l s t -> IO (t, l)
evalLIO s m = do (a, s) <- (runLIO m $ newLIO s)
                 return (a, lioL s)

getL             :: Label l => Lref l a -> LIO l s a
getL (Lref la a) = do
  s <- get
  if la `leq` lioC s
    then do put s { lioL = lioL s `lub` la }
            return a
    else
        return undefined

putL   :: (Label l) => LIO l s a -> LIO l s (Lref l a)
putL m = do
  LIOstate { lioL = l } <- get
  a <- m
  s <- get
  put s { lioL = l }
  return $ Lref (lioL s) a

discard m = putL m >> return ()
  
-- For use in the TCB
lio :: (Label l) => l -> IO a -> LIO l s a
lio l a = mkLIO $ \s -> do
            r <- a
            return (r, s { lioL = lioL s `lub` l })

lputStr x = lio lpure $ putStr x
lputStrLn x = lio lpure $ putStrLn x

