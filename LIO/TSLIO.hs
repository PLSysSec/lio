{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module TSLIO where

import LIO.TCB ( POrdering(..), POrd(..), o2po, Label(..)
               , Priv(..), NoPrivs(..)
               , ShowTCB(..)
               , ReadTCB(..)
               , PrivTCB, MintTCB(..)
               )
import Control.Monad.State.Lazy hiding (put, get)
import Control.Concurrent
import qualified Control.Exception as E

import LIO.DCLabel hiding (DC,evalDC)
import DCLabel.PrettyShow

--
import System.CPUTime
--


data TSLIOstate l s =
    TSLIOstate { labelState :: s
               , tslioL  :: l -- current label
               , tslioEC :: l -- current effective clearance
               , tslioC  :: l -- current clearance
               }

newtype (Label l) => TSLIO l s a = TSLIO (StateT (TSLIOstate l s) IO a)
    deriving (Functor, Monad, MonadFix)

get :: (Label l) => TSLIO l s (TSLIOstate l s)
get = mkTSLIO $ \s -> return (s, s)

put :: (Label l) => TSLIOstate l s -> TSLIO l s ()
put s = mkTSLIO $ \_ -> return (() , s)

mkTSLIO :: (Label l) => (TSLIOstate l s
        -> IO (a, TSLIOstate l s)) -> TSLIO l s a
mkTSLIO = TSLIO . StateT

unTSLIO :: (Label l) => TSLIO l s a -> TSLIOstate l s
        -> IO (a, TSLIOstate l s)
unTSLIO (TSLIO (StateT f)) = f

runTSLIO :: forall l s a. (Label l) => TSLIO l s a -> TSLIOstate l s
         -> IO (a, TSLIOstate l s)
runTSLIO m s = unTSLIO m s -- TOD: exception catching

evalTSLIO :: (Label l)
          => TSLIO l s a -- ^ The TSLIO computation to execute
          -> s           -- ^ Initial value of label-specific state
          -> IO (a, l)   -- ^ IO computation that will execute first argument
evalTSLIO m s = do (a, ls) <- runTSLIO m (newstate s)
                   return (a, tslioL ls)
  where newstate :: (Label l) => s -> TSLIOstate l s
        newstate s = TSLIOstate { labelState = s
                                , tslioL  = lbot
                                , tslioEC = ltop
                                , tslioC  = ltop }

ioTCB :: (Label l) => IO a -> TSLIO l s a
ioTCB a = mkTSLIO $ \s -> do r <- a; return (r, s)

gtaint :: (Label l) =>
          (l -> l -> l)         -- ^ @mylub@ function
       -> l                     -- ^ @l@ - Label to taint with
       -> TSLIO l s ()
gtaint mylub l = do
  s <- get
  let lnew = l `mylub` (tslioL s)
  if lnew `leq` tslioEC s
     then put s { tslioL = lnew }
     else ioTCB $ E.throwIO $ userError "LerrClr"

taint :: (Label l) => l -> TSLIO l s ()
taint = gtaint lub


forkTSLIO :: (Label l) => TSLIO l s () -> TSLIO l s ThreadId
forkTSLIO m = get >>= \s ->
  ioTCB . forkIO $ runTSLIO m s >> return ()

type ErrorStr = String -- for now
data LFuture l a = LFutureTCB l (MVar (Either ErrorStr a))

labeledFuture :: Label l => l -> TSLIO l s a -> TSLIO l s (LFuture l a)
labeledFuture l m = do
  s <- get
  guardL s l
  mv <- ioTCB newEmptyMVar
  forkTSLIO $ do put (s { tslioEC = l })
                 res <- m -- catch etc etc.
                 ioTCB $ putMVar mv (Right res)
  return $ LFutureTCB l mv
    where guardL s l | not (tslioL s `leq` l) = ioTCB $ E.throwIO $ userError "LerrLow"
                     | not (l `leq` tslioC s) = ioTCB $ E.throwIO $ userError "LerrClr"
                     | otherwise              = return ()

openLFuture :: Label l => LFuture l a -> TSLIO l s a
openLFuture (LFutureTCB l mv) = do
  taint l
  v <- ioTCB $ takeMVar mv
  case v of
    Left e -> ioTCB $ E.throwIO $ userError e
    Right v' -> return v'


-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------

type DC = TSLIO DCLabel ()

evalDC :: DC a -> IO (a, DCLabel)
evalDC m = evalTSLIO m ()

h = newDC ("H") (<>)

test = niceEvalDC $ do
  f <- labeledFuture h $ return (2 + 3)
  f' <- labeledFuture h $ do forever (return ())
                             return 3
  openLFuture f

niceEvalDC :: Show a => DC a -> IO ()
niceEvalDC m = do
  (a, l) <- evalDC m
  putStrLn (show a)
  putStrLn (prettyShow l)


getSimpleDiff :: DC Integer
getSimpleDiff = ioTCB $ do
  t1 <- getCPUTime
  t2 <- getCPUTime
  return $ t2 - t1

test2 = niceEvalDC $ do
  f <- labeledFuture h $ do
          ioTCB $ threadDelay 1
          --sequence $ replicate 10000 $ labeledFuture  h $ do forever (return ())
          return 3
  tdiffs <- sequence $ replicate 10000 getSimpleDiff
  ioTCB . putStrLn $ show $ filter (/=0) tdiffs
  openLFuture f
