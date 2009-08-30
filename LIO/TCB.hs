{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XExistentialQuantification #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}


module LIO.TCB ( 
                -- * Basic label functions
                 POrdering(..), POrd(..), o2po, Label(..)
               , Lref, Priv(..)
               , labelOf, taint, untaint, unlref
               -- * Labeled IO Monad (LIO)
               , LIO
               , lref
               , labelOfio, clearOfio
               , taintio, guardio, cleario, untaintio
               , ptaintio, pguardio
               , lowerio, unlowerio
               , openL, closeL, discardL
               -- * Exceptions
               , throwL, catchL, catchLp
               , LabelFault(..)
               -- Start TCB exports
               -- * Privileged operations
               , lrefTCB
               , PrivTCB
               , showTCB
               , unlrefTCB, untaintioTCB, unlowerioTCB
               , getTCB, putTCB, runTCB, evalTCB
               , ioTCB, rtioTCB
               , rethrowTCB
               -- End TCB exports
               ) where

import Prelude hiding (catch)
import Control.Monad.State.Lazy hiding (put, get)
import Control.Exception
import Data.Monoid
import Data.Typeable

{- Things to worry about:

   - unsafe... functions must be blocked

   - inlinePerformIO must be blocked

   - Allowing untrusted code to define instances Typeable with bogus
     typeOf functions could lead to unsafe casts.

   - Some way of showing an Lref or even just the label, by putting it
     into an exception.

 -}

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

o2po :: Ordering -> POrdering
o2po EQ = PEQ; o2po LT = PLT; o2po GT = PGT
-- instance (Ord a) => POrd a where pcompare = o2po . compare

class (POrd a, Show a, Read a, Typeable a) => Label a where
    lpure :: a                  -- label for pure values
    lsys :: a                   -- label for unlabeled system files
    lsys = lpure
    lclear :: a                 -- default clearance
    lub :: a -> a -> a
    glb :: a -> a -> a


--
-- Labeled value - Lref
-- Downgrading privileges - Priv
--

data (Label l) => Lref l t = Lref l t

showTCB            :: (Label l, Show t) => Lref l t -> String
showTCB (Lref l t) = shows t $ " {" ++ shows l "}"

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
    -- |@leqp p l1 l2@ means that privileges @p@ are sufficient to
    -- downgrade data from @l1@ to @l2@.  Note that @'leq' l1 l2@
    -- implies @'leq' p l1 l2@ for all @p@, but for some labels and
    -- @p@ values @leqp@ will hold even if @'leq'@ does not.
    leqp :: p -> l -> l -> Bool
    leqp p a b = lostar p a b `leq` b

    -- |@lostar p source minimum@ returns the lowest label to which
    -- one can downgrade data labeled @source@ given privileges @p@,
    -- least-upper-bounded with @minimum@.  (Without @minimum@, the
    -- lowest label might be exponential in @p@ for some label
    -- formats.)  More concretely, the result returned is the lowest
    -- @lres@ such that:  @'leqp' p source lres && 'leq' minimum lres@
    --
    -- This is useful if your label is originally @l1@, and you touch
    -- some stuff labeled @l2@ but want to minimize the amount of
    -- taint @l2@ causes you.  After raising your label to @l2@, you
    -- can use the privileges in @p@ to lower your label to @lostar p
    -- l2 l1@.
    lostar :: p -> l -> l -> l

lrefTCB     :: Label l => l -> a -> Lref l a
lrefTCB l a = Lref l a

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

data (Label l) => LIOstate l s =
    LIOstate { labelState :: s
             , lioL :: l -- current label
             , lioC :: l -- current clearance
             }

newtype (Label l) => LIO l s a = LIO (StateT (LIOstate l s) IO a)
    deriving (Functor, Monad, MonadFix)

get :: (Label l) => LIO l s (LIOstate l s)
get = mkLIO $ \s -> return (s, s)

put :: (Label l) => LIOstate l s -> LIO l s ()
put s = mkLIO $ \_ -> return (() , s)

lref     :: (Label l) => l -> a -> LIO l s (Lref l a)
lref l a = get >>= doit
    where doit s | not $ l `leq` lioC s = throwL LerrClearance
                 | not $ lioL s `leq` l = throwL LerrLow
                 | otherwise            = return $ Lref l a

labelOfio :: (Label l) => LIO l s l
labelOfio = get >>= return . lioL

clearOfio :: (Label l) => LIO l s l
clearOfio = get >>= return . lioC

-- |Use @taintio l@ in trusted code before observing an object labeled
-- @l@.  This will raise the current label to a value @l'@ such that
-- @l ``leq`` l'@, or throw @'LerrClearance'@ if @l'@ would have to be
-- higher than the current clearance.
taintio    :: (Label l) => l -> LIO l s ()
taintio l' = do s <- get
                let l = lioL s `lub` l'
                if l `leq` lioC s
                  then put s { lioL = l }
                  else throwL LerrClearance

-- |Like 'taintio', but use privileges to reduce the amount of taint
-- |required.
ptaintio      :: (Priv l p) =>
                 p              -- ^Privileges to invoke
              -> l              -- ^Label to taint to if no privileges
              -> LIO l s ()
ptaintio p l' = do s <- get
                   let l = lostar p l' (lioL s)
                   if l `leq` lioC s
                     then put s { lioL = l }
                     else throwL LerrClearance

-- |Use @guardio l@ in trusted code before modifying an object labeled
-- @l@.  If @l'@ is the current label, then this function ensures that
-- @l' ``leq`` l@ before doing the same thing as @'ltaintio' l@.
-- Throws @'LerrHigh'@ if the current label @l'@ is too high.
guardio :: (Label l) => l -> LIO l s ()
guardio l = do l' <- labelOfio
               if l' `leq` l
                 then taintio l
                 else throwL LerrHigh

-- |Like 'guardio', but takes privilege argument to be more permissive.
pguardio     :: (Priv l p) => p -> l -> LIO l s ()
pguardio p l = do l' <- labelOfio
                  if leqp p l' l
                    then ptaintio p l
                    else throwL LerrHigh

-- |Ensures the clearance is at least a certain level, or throw
-- 'LerrClearance'.
cleario :: (Label l) => l -> LIO l s ()
cleario min = do c <- clearOfio
                 if min `leq` c
                   then return ()
                   else throwL LerrClearance

untaintio     :: (Priv l p) => p -> l -> LIO l s ()
untaintio p l = do s <- get
                   if leqp p (lioL s) l
                     then put s { lioL = l }
                     else throwL LerrPriv

untaintioTCB     :: (Label l) => l -> LIO l s ()
untaintioTCB l = do s <- get
                    if l `leq` lioC s
                      then put s { lioL = l }
                      else throwL LerrClearance

lowerio   :: (Label l) => l -> LIO l s ()
lowerio l = get >>= doit
    where doit s | not $ l `leq` lioC s = throwL LerrClearance
                 | not $ lioL s `leq` l = throwL LerrLow
                 | otherwise            = put s { lioC = l }

unlowerio   :: (Priv l p) => p -> l -> LIO l s ()
unlowerio p l = get >>= doit
    where doit s | not $ leqp p l $ lioC s = throwL LerrPriv
                 | not $ lioL s `leq` l = throwL LerrLow
                 | otherwise            = put s { lioC = l }

unlowerioTCB   :: (Label l) => l -> LIO l s ()
unlowerioTCB l = get >>= doit
    where doit s | not $ lioL s `leq` l = throwL LerrInval
                 | otherwise            = put s { lioC = l }

openL             :: (Label l) => Lref l a -> LIO l s a
openL (Lref la a) = do
  s <- get
  if la `leq` lioC s
    then do put s { lioL = lioL s `lub` la }
            return a
    else
        return undefined

-- Might have lowered clearance inside closeL, so just preserve it
closeL   :: (Label l) => LIO l s a -> LIO l s (Lref l a)
closeL m = do
  LIOstate { lioL = l, lioC = c } <- get
  a <- m
  s <- get
  put s { lioL = l, lioC = c }
  return $ Lref (lioL s) a

discardL m = closeL m >> return ()
  

getTCB :: (Label l) => LIO l s s
getTCB = get >>= return . labelState

putTCB    :: (Label l) => s -> LIO l s ()
putTCB ls = get >>= put . update
    where update s = s { labelState = ls }

newstate   :: (Label l) => s -> LIOstate l s
newstate s = LIOstate { labelState = s , lioL = lpure , lioC = lclear }

mkLIO :: (Label l) => (LIOstate l s -> IO (a, LIOstate l s)) -> LIO l s a
mkLIO = LIO . StateT

unLIO                  :: (Label l) => LIO l s a -> LIOstate l s
                       -> IO (a, LIOstate l s)
unLIO (LIO (StateT f)) = f

runLIO     :: (Label l) => LIO l s a -> LIOstate l s
           -> IO (a, LIOstate l s)
runLIO m s = unLIO m s `catch` unlabelException

runTCB     :: (Label l) => LIO l s a -> s -> IO (a, s)
runTCB m s = do (a, ls) <- runLIO m (newstate s)
                return (a, labelState ls)

evalTCB     :: (Label l) => LIO l s a -> s -> IO (a, l)
evalTCB m s = do (a, ls) <- runLIO m (newstate s)
                 return (a, lioL ls)

ioTCB :: (Label l) => IO a -> LIO l s a
ioTCB a = mkLIO $ \s -> do r <- a; return (r, s)

rtioTCB :: (Label l) => IO a -> LIO l s a
rtioTCB a = rethrowTCB $ mkLIO $ \s -> do r <- a; return (r, s)

iotTCB     :: (Label l) =>
              (IO (a, LIOstate l s) -> IO (a, LIOstate l s)) -> LIO l s a
           -> LIO l s a
iotTCB f m = mkLIO $ \s -> f (unLIO m s)

blockL :: (Label l) => LIO l s a -> LIO l s a
blockL m = mkLIO $ \s -> block (unLIO m s)

unblockL :: (Label l) => LIO l s a -> LIO l s a
unblockL m = mkLIO $ \s -> unblock (unLIO m s)


--
-- Exceptions
--

data LabelFault
    = LerrLow                   -- ^Requested label too low
    | LerrHigh                  -- ^Current label too high
    | LerrClearance             -- ^Label would exceed clearance
    | LerrPriv                  -- ^Insufficient privileges
    | LerrInval                 -- ^Invalid request
      deriving (Show, Typeable)

instance Exception LabelFault

data LabeledExceptionTCB l =
    LabeledExceptionTCB l SomeException deriving Typeable

instance Label l => Show (LabeledExceptionTCB l) where
    showsPrec _ (LabeledExceptionTCB l e) rest =
        shows e $ (" {" ++) $ shows l $ "}" ++ rest

instance (Label l) => Exception (LabeledExceptionTCB l)

unlabelException :: (Label l) => LabeledExceptionTCB l -> IO (a, LIOstate l s)
unlabelException (LabeledExceptionTCB l (SomeException e)) =
    putStrLn ("unlabeling " ++ show e ++ " {" ++ show l ++ "}") >> -- XXX
    throw e

throwL   :: (Exception e, Label l) => e -> LIO l s a
throwL e = mkLIO $ \s -> throwIO $
           LabeledExceptionTCB (lioL s) (toException e)

rethrowTCB   :: (Label l) => LIO l s a -> LIO l s a
rethrowTCB m = do
  l <- labelOfio
  fmap (mapException $ labelit l) m
      where
        labelit     :: Label l => l -> SomeException -> LabeledExceptionTCB l
        labelit l e = LabeledExceptionTCB l e

catchLp       :: (Label l, Exception e, Priv l p) =>
                 LIO l s a -> p -> (l -> e -> LIO l s a) -> LIO l s a
catchLp m p c = mkLIO $ \s -> unLIO m s `catch` doit s
    where doit s e@(LabeledExceptionTCB l se) =
              case fromException se of
                Just e' | leqp p l $ lioL s -> unLIO (c l e') s
                Nothing -> throw e

catchL     :: (Label l, Exception e) =>
              LIO l s a -> (e -> LIO l s a) -> LIO l s a
catchL m c = mkLIO $ \s -> unLIO m s `catch` doit s
    where doit s e@(LabeledExceptionTCB l se) =
              case fromException se of
                Just e' | l `leq` lioL s -> unLIO (c e') s
                Nothing -> throw e
