{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |This module implements the core (Trusted Computing Base) of the
-- Labeled IO library for information flow control in Haskell.  It
-- provides a data structure 'Lref' (labeled reference), which
-- protects access to pure values.  Without the appropriate
-- privileges, one cannot produce a pure value that depends on a
-- secret 'Lref', or conversely produce a high-integrity 'Lref' based
-- on pure data.
--
-- The module also provides a monad, 'LIO', to allow untrusted code to
-- perform IO actions.  The idea is that untrusted code can provide a
-- computation in the 'LIO' monad, and trusted code can then safely
-- execute this code via 'evalLIO', though usually a wrapper function
-- is employed depending on the type of labels used by an application.
-- For example, with "LIO.DCLabels", you would use 'evalDC' to execute
-- an untrusted computation.
--
-- Untrusted code must be prevented from importing this module.  The
-- exported symbols ending ...@TCB@ can be used to violate label
-- protections even from within pure code or the LIO Monad.  A safe
-- subset of these symbols is exported by the "LIO.Base" module, which
-- is how untrusted code should access the core label functionality.
module LIO.TCB (
                -- * Basic label functions
                -- $labels
                 POrdering(..), POrd(..), o2po, Label(..)
               , Lref, Priv(..), NoPrivs(..)
               , taintR, setLabelRP, unlrefP
               -- * Labeled IO Monad (LIO)
               , LIO
               , lref
               , labelOfio, clearOfio
               , taintio, guardio, cleario, untaintio
               , ptaintio, pguardio
               , lowerio, unlowerio
               , openL, closeL, discardL
               -- * Exceptions
               , throwL, catchL, catchLp, onExceptionL
               , LabelFault(..)
               , MonadBlock(..)
               -- * Executing computations
               , evalLIO
               -- Start TCB exports
               -- * Privileged operations
               , lrefTCB
               , PrivTCB, MintTCB(..)
               , showTCB
               , unlrefTCB, untaintioTCB, unlowerioTCB
               , getTCB, putTCB
               , ioTCB, rtioTCB
               , rethrowTCB, OnExceptionTCB(..)
               -- End TCB exports
               ) where

import Prelude hiding (catch)
import Control.Monad.State.Lazy hiding (put, get)
import Control.Exception
import Data.Monoid
import Data.Typeable

{- Things to worry about:

   - Because code can look at an Lref, it is possible to leak data in
     Lclose.  For example, look at a secret bit, and if it is 1, then
     taint yourself in a whole bunch more categories.

   - Similar to above, but even without Lclose, just taint yourself
     within the Lref.

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

{- $labels

Labels are a way of describing who can observe and modify data.  There
is a partial order, generally pronounced \"can flow to\" on labels.
The idea is that an object labeled L1 should affect one labeled L2
only if L1 /can flow to/ L2.  This can flow to relation is expressed
by the `leq` function (i.e., less then or equal to).  Note that higher
labels are not more-or-less privileged.  As a general rule of thumb,
the higher your label, the more things you can read, while the lower
the label, the more things you can write.

Privilege comes from a separate class, called 'Priv', representing the
ability to bypass the protection of certain labeles.  Given
privileges, one might be able to copy data from an object labeled L1
to one labeled L2, but which specific pairs depends on the specific
privileges.

-}

data POrdering = PEQ            -- ^Equal
               | PLT            -- ^Less than
               | PGT            -- ^Greater than
               | PNE
                 -- ^Incomparable (neither less than nor greater than)
                 deriving (Eq, Ord, Show)

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
    -- | label for pure values
    lpure :: a
    -- | default /clearance/ (highest value one can raise one's label to)
    lclear :: a
    -- | least upper bound (join) of two labels
    lub :: a -> a -> a
    -- | greatest lower bound (meet) of two labels
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

-- |PrivTCB is a method-less class whose only purpose is to be
-- unavailable to unprivileged code.  Since (PrivTCB t) => is in the
-- context of class 'Priv', this ensures that, since unprivileged code
-- cannot create new instances of the PrivTCB class, it won't be able
-- to create new instances of the 'Priv' class either, even though the
-- symbol 'Priv' is exported by "LIO.Base".
class PrivTCB t where
class (PrivTCB t) => MintTCB t i where
    -- |The function that mints new privileges.
    mintTCB :: i -> t
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

-- |A generic 'Priv' instance that works for all 'Label's and confers
-- no downgrading privileges.
data NoPrivs = NoPrivs
instance PrivTCB NoPrivs
instance Monoid NoPrivs where
    mempty      = NoPrivs
    mappend _ _ = NoPrivs
instance (Label l) => Priv l NoPrivs where
    leqp _ a b     = leq a b
    lostar _ l min = lub l min

lrefTCB     :: Label l => l -> a -> Lref l a
lrefTCB l a = Lref l a

--
-- Since this function has a covert channel, don't export for now.
-- Really what we need is a way to check that an LRef has some
-- particular integrity category (or is below some label that we are
-- allowed to read).
--
labelOfR            :: Label l => Lref l a -> l
labelOfR (Lref l a) = l

taintR               :: (Label l) => l -> Lref l a -> Lref l a
taintR l' (Lref l a) = Lref (lub l l') a

guardR :: (Label l) => l -> Lref l a -> LIO l s ()
guardR l' (Lref l a) = do
  cur <- labelOfio
  unless (l `leq` l' && l `leq` cur) $ throwL LerrHigh

setLabelRP                   :: Priv l p => p -> l -> Lref l a -> Lref l a
setLabelRP p newl (Lref l a) = if leqp p l newl then Lref newl a else undefined

unlrefP              :: Priv l p => p -> Lref l a -> a
unlrefP p (Lref l a) = if leqp p l lpure then a else undefined

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
-- required.
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

-- |Ensures the label argument is between the current IO label and
-- current IO clearance.  Use this function in code that allocates
-- objects--you shouldn't be able to create an object labeled @l@
-- unless @cleario l@ does not throw an exception.
cleario :: (Label l) => l -> LIO l s ()
cleario newl = do c <- clearOfio
                  l <- labelOfio
                  unless (leq newl c) $ throwL LerrClearance
                  unless (leq l newl) $ throwL LerrLow
                  return ()

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

-- |Reduce the current clearance.  One cannot raise the current label
-- or create object with labels higher than the current clearance.
lowerio   :: (Label l) => l -> LIO l s ()
lowerio l = get >>= doit
    where doit s | not $ l `leq` lioC s = throwL LerrClearance
                 | not $ lioL s `leq` l = throwL LerrLow
                 | otherwise            = put s { lioC = l }

-- |Raise the current clearance (undoing the effects of 'lowerio').
-- This requires privileges.
unlowerio   :: (Priv l p) => p -> l -> LIO l s ()
unlowerio p l = get >>= doit
    where doit s | not $ leqp p l $ lioC s = throwL LerrPriv
                 | not $ lioL s `leq` l = throwL LerrLow
                 | otherwise            = put s { lioC = l }

unlowerioTCB   :: (Label l) => l -> LIO l s ()
unlowerioTCB l = get >>= doit
    where doit s | not $ lioL s `leq` l = throwL LerrInval
                 | otherwise            = put s { lioC = l }

-- |Within the 'LIO' monad, this function takes an 'Lref' and returns
-- the value.  Thus, in the 'LIO' monad one can say:
--
-- > x <- openL (xref :: Lref SomeLabelType Int)
--
-- And now it is possible to use the value of @x@, which is the pure
-- value of what was stored in @xref@.  Of course, @openL@ also raises
-- the current label.  If raising the label would exceed the current
-- clearance, then the value of @x@ is undefined, which means the
-- value of any computation that actually uses @x@ will also be
-- undefined.
openL             :: (Label l) => Lref l a -> LIO l s a
openL (Lref la a) = do
  s <- get
  if la `leq` lioC s
    then do put s { lioL = lioL s `lub` la }
            return a
    else
        return undefined

-- Might have lowered clearance inside closeL, so just preserve it
-- |@closeL@ is the dual of @openL@.  It allows one to invoke
-- computations that would raise the current label, but without
-- actually raising the label.  Instead, the result of the computation
-- is packaged into an 'Lref'.  Thus, to get at the result of the
-- computation one will have to call 'openL' and raise the label, but
-- this can be postponed, or done inside some other call to 'closeL'.
closeL   :: (Label l) => LIO l s a -> LIO l s (Lref l a)
closeL m = do
  LIOstate { lioL = l, lioC = c } <- get
  a <- m
  s <- get
  put s { lioL = l, lioC = c }
  return $ Lref (lioL s) a

-- |Executes a computation that would raise the current label, but
-- discards the result so as to keep the label the same.
discardL   :: (Label l) => LIO l s a -> LIO l s ()
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

runLIO     :: forall l s a. (Label l) => LIO l s a -> LIOstate l s
           -> IO (a, LIOstate l s)
runLIO m s = unLIO m s `catch` (throw . delabel)
    where delabel :: LabeledExceptionTCB l -> SomeException
          delabel (LabeledExceptionTCB l e) = e
           -- trace ("unlabeling " ++ show e ++ " {" ++ show l ++ "}") e

evalLIO     :: (Label l) => LIO l s a -> s -> IO (a, l)
evalLIO m s = do (a, ls) <- runLIO m (newstate s)
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
    = LerrLow                   -- ^ Requested label too low
    | LerrHigh                  -- ^ Current label too high
    | LerrClearance             -- ^ Label would exceed clearance
    | LerrPriv                  -- ^ Insufficient privileges
    | LerrInval                 -- ^ Invalid request
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
    throwIO e

-- |It is not possible to catch pure exceptions from within the 'LIO'
-- monad, but @throwL@ wraps up an exception with the current label,
-- so that it can be caught with 'catchL'.
throwL   :: (Exception e, Label l) => e -> LIO l s a
throwL e = mkLIO $ \s -> throwIO $
           LabeledExceptionTCB (lioL s) (toException e)

-- | Map a function over the underlying IO computation within an LIO.
-- Obviously this symbol should not be exported, as it is privileged.
iomaps     :: (Label l) =>
              ((LIOstate l s) -> IO (a, LIOstate l s) -> IO (a, LIOstate l s))
           -> LIO l s a -> LIO l s a
iomaps f c = mkLIO $ \s -> (f s $ unLIO c s)

-- | Like 'iomaps' if you don't need the state.
iomap   :: (Label l) =>
           (IO (a, LIOstate l s) -> IO (a, LIOstate l s))
        -> LIO l s a -> LIO l s a
iomap f = iomaps $ \_ -> f

-- |Privileged code that does IO operations may cause exceptions that
-- should be caught by untrusted code in the 'LIO' monad.  Such
-- operations should be wrapped by @rethrowTCB@ (or 'rtioTCB', which
-- uses @rethrowTCB@) to ensure the exception is labeled.  Note that
-- it is very important that the computation executed inside
-- @rethrowTCB@ not in any way change the label, as otherwise
-- @rethrowTCB@ would put the wrong label on the exception.
rethrowTCB :: (Label l) => LIO l s a -> LIO l s a
rethrowTCB = iomaps $ \s -> handle (throwIO . (LabeledExceptionTCB $ lioL s))
 -- mapException (LabeledExceptionTCB $ lioL s) c

-- | Catches an exception, so long as the label at the point where the
-- exception was thrown can flow to the label at which catchLp is
-- invoked, modulo the privileges specified.  Note that the handler
-- receives an an extra first argument (before the exception), which
-- is the label when the exception was thrown.
catchLp       :: (Label l, Exception e, Priv l p) =>
                 LIO l s a             -- ^ Computation to run
              -> p   -- ^ Privileges with which to downgrade exception
              -> (l -> e -> LIO l s a) -- ^ Exception handler
              -> LIO l s a             -- ^ Result of computation or handler
catchLp m p c = iomaps (\s m' -> m' `catch` doit s) m
    where doit s e@(LabeledExceptionTCB l se) =
              case fromException se of
                Just e' | leqp p l $ lioL s -> unLIO (c l e') s
                Nothing -> throw e

-- | Basic function for catching labeled exceptions.  (The fact that
-- they are labeled is hidden from the handler.)
--
-- > catchL m c = catchLp m NoPrivs (\_ -> c)
--
catchL     :: (Label l, Exception e) =>
              LIO l s a -> (e -> LIO l s a) -> LIO l s a
catchL m c = iomaps (\s m' -> m' `catch` doit s) m
    where doit s e@(LabeledExceptionTCB l se) =
              case fromException se of
                Just e' | l `leq` lioL s -> unLIO (c e') s
                Nothing -> throw e

-- | Analogous to 'onException', but for the 'LIO' monad.  Note,
-- however, that the handler will not run if the label is raised.
onExceptionL         :: (Label l) =>
                        LIO l s a -> LIO l s b -> LIO l s a
onExceptionL io what = io `catchL` \e -> do what
                                            throwL (e :: SomeException)

onExceptionLp           :: (Priv l p) =>
                           LIO l s a -> p -> LIO l s b -> LIO l s a
onExceptionLp io p what = catchLp io p
                          (\l e -> what >> throwL (e :: SomeException))

-- | @MonadBlock@ is the class of monads that support the 'block' and
-- 'unblock' functions for disabling and enabling asynchronous
-- exceptions, respectively.  The generalized methods are named
-- 'blockM' and 'unblockM'
class (Monad m) => MonadBlock m where
    blockM   :: m a -> m a
    unblockM :: m a -> m a
instance MonadBlock IO where
    blockM   = block
    unblockM = unblock
instance (Label l) => MonadBlock (LIO l s) where
    blockM   = iomap block
    unblockM = iomap unblock

-- | For privileged code that needs to catch all exceptions in some
-- cleanup function.  Note that for the 'LIO' monad, this function
-- does /not/ call 'rethrowTCB' to label the exceptions.  It is
-- assumed that you will use 'rtioTCB' for IO within the computation.
class (MonadBlock m) => OnExceptionTCB m where
    onExceptionTCB :: m a -> m b -> m a
    bracketTCB     :: m a -> (a -> m b) -> (a -> m c) -> m c
    bracketTCB before after between =
        blockM $ do
          a <- before
          b <- unblockM (between a) `onExceptionTCB` after a
          after a
          return b
instance OnExceptionTCB IO where
    onExceptionTCB = onException
    bracketTCB     = bracket
instance (Label l) => OnExceptionTCB (LIO l s) where
    onExceptionTCB io cleanup = 
        mkLIO $ \s -> unLIO io s
                      `catch` \e -> do unLIO cleanup s
                                       throwIO (e :: SomeException)

