{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#else
#warning "This module is not using SafeHaskell"
#endif
-- |This module implements labeled fork and wait.
module LIO.Concurrent ( lForkP, lFork
                      , lWaitP, lWait
                      ) where

import LIO.TCB hiding (toLabeledP, toLabeled)
import LIO.Concurrent.LMVar.TCB
import Control.Concurrent

-- | Just an "LIO" fork.
forkLIO :: (Label l) => LIO l s () -> LIO l s ThreadId
forkLIO m = getTCB >>= \s ->
  ioTCB . forkIO $ evalLIO m s >> return ()

data LRes l a = LRes (LMVar l a)

-- | Labeled fork with privileges.
lForkP :: Priv l p => p -> l -> LIO l s a -> LIO l s (LRes l a)
lForkP p l m = do
  mv <- newEmptyLMVarP p l
  forkLIO $ do res <- m 
               putLMVarP p mv res
  return $ LRes mv

-- | Labeled fork.
lFork :: Label l => l -> LIO l s a -> LIO l s (LRes l a)
lFork = lForkP NoPrivs

-- | Labeled wait with privileges.
lWaitP :: Priv l p => p -> LRes l a -> LIO l s a
lWaitP p (LRes mv) = takeLMVarP p mv

-- | Labeled wait.
lWait :: Label l => LRes l a -> LIO l s a
lWait = lWaitP NoPrivs
