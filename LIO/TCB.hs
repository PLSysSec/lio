{-# OPTIONS_GHC -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving #-}

module LIO.TCB ( POrdering(..), POrd(..), o2po, Label(..)
               , Lref, Priv(..)
               , lref, labelOf, taint, untaint, unlref
               , LIO
               , labelOfio, clearOfio
               , taintio, guardio, untaintio
               , lowerio, unlowerio
               , openL, closeL, discardL
               -- Start TCB exports
               , PrivTCB
               , unlrefTCB, untaintioTCB, unlowerioTCB
               , getTCB, putTCB, runTCB, evalTCB
               , ioTCB
               -- End TCB exports
               ) where

import Control.Monad.State.Lazy hiding (put, get)
import Data.Monoid


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
-- Downgrading privileges - Priv
--

data (Label l) => Lref l t = Lref l t deriving (Show)

instance Label l => Functor (Lref l) where
    fmap f (Lref l t) = Lref (l `lub` lpure) (f t)

instance Label l => Monad (Lref l) where
    return x = Lref lpure x
    (Lref l x) >>= f = let Lref l' y = f x in Lref (lub l l') y

instance Label l => MonadFix (Lref l) where
    mfix f = fix g
        where g ~(Lref _ a) = f a

class PrivTCB t where
class (Label l, Monoid p, PrivTCB p) => Priv l p where
    leqp :: p -> l -> l -> Bool

lref     :: Label l => l -> a -> Lref l a
lref l a = Lref (lub l lpure) a

labelOf            :: Label l => Lref l a -> l
labelOf (Lref l a) = l

taint               :: (Label l) => l -> Lref l a -> Lref l a
taint l' (Lref l a) = Lref (lub l l') a

untaint                   :: Priv l p => p -> l -> Lref l a -> Lref l a
untaint p newl (Lref l a) = if leqp p l newl then Lref newl a else undefined

unlref              :: Priv l p => p -> Lref l a -> a
unlref p (Lref l a) = if leqp p l lpure then a else undefined

unlrefTCB            :: Label l => Lref l a -> a
unlrefTCB (Lref l a) = a


--
-- Labeled IO
--

data LIOstate l s = LIOstate { labelState :: s
                             , lioL :: l -- current label
                             , lioC :: l -- current clearance
                             }

newtype LIO l s a = LIO (StateT (LIOstate l s) IO a)
    deriving (Functor, Monad, MonadFix)

get :: Label l => LIO l s (LIOstate l s)
get = mkLIO $ \s -> return (s, s)

put :: Label l => LIOstate l s -> LIO l s ()
put s = mkLIO $ \_ -> return (() , s)

labelOfio :: Label l => LIO l s l
labelOfio = get >>= return . lioL

clearOfio :: Label l => LIO l s l
clearOfio = get >>= return . lioC

taintio    :: Label l => l -> LIO l s ()
taintio l' = do s <- get
                let l = lioL s `lub` l'
                if l `leq` lioC s
                  then put s { lioL = l }
                  else fail "Taint exceeds clearance"

guardio :: Label l => l -> LIO l s ()
guardio max = do l <- labelOfio
                 if l `leq` max
                   then return ()
                   else fail "guardio failed"

untaintio     :: Priv l p => p -> l -> LIO l s ()
untaintio p l = do s <- get
                   if leqp p (lioL s) l
                     then put s { lioL = l }
                     else fail "Insufficient privilege for untaintio"

untaintioTCB     :: Label l => l -> LIO l s ()
untaintioTCB l = do s <- get
                    if l `leq` lioC s
                      then put s { lioL = l }
                      else fail "Untaintio exceeds clearance"

lowerio   :: Label l => l -> LIO l s ()
lowerio l = get >>= doit
    where doit s | not $ l `leq` lioC s = fail "Cannot raise with lower"
                 | not $ lioL s `leq` l = fail "Cannot lower below label"
                 | otherwise            = put s { lioC = l }

unlowerio   :: Priv l p => p -> l -> LIO l s ()
unlowerio p l = get >>= doit
    where doit s | not $ leqp p l $ lioC s = fail "Unlower denied"
                 | not $ lioL s `leq` l = fail "Cannot unlower below label"
                 | otherwise            = put s { lioC = l }

unlowerioTCB   :: Label l => l -> LIO l s ()
unlowerioTCB l = get >>= doit
    where doit s | not $ lioL s `leq` l = fail "Cannot unlowerTCB below label"
                 | otherwise            = put s { lioC = l }

openL             :: Label l => Lref l a -> LIO l s a
openL (Lref la a) = do
  s <- get
  if la `leq` lioC s
    then do put s { lioL = lioL s `lub` la }
            return a
    else
        return undefined

closeL   :: (Label l) => LIO l s a -> LIO l s (Lref l a)
closeL m = do
  LIOstate { lioL = l } <- get
  a <- m
  s <- get
  put s { lioL = l }
  return $ Lref (lioL s) a

discardL m = closeL m >> return ()
  

getTCB :: Label l => LIO l s s
getTCB = get >>= return . labelState

putTCB    :: Label l => s -> LIO l s ()
putTCB ls = get >>= put . update
    where update s = s { labelState = ls }

newLIO   :: Label l => s -> LIOstate l s
newLIO s = LIOstate { labelState = s , lioL = lpure , lioC = lclear }

runTCB                    :: Label l => LIO l s a -> s -> IO (a, s)
runTCB (LIO (StateT f)) s = do (a, ls) <- f (newLIO s)
                               return (a, labelState ls)

evalTCB                    :: (Label l) => LIO l s a -> s -> IO (a, l)
evalTCB (LIO (StateT f)) s = do (a, ls) <- f (newLIO s)
                                return (a, lioL ls)

mkLIO :: Label l => (LIOstate l s -> IO (a, LIOstate l s)) -> LIO l s a
mkLIO = LIO . StateT

ioTCB :: (Label l) => IO a -> LIO l s a
ioTCB a = mkLIO $ \s -> do r <- a; return (r, s)

lputStr x = ioTCB $ putStr x
lputStrLn x = ioTCB $ putStrLn x
