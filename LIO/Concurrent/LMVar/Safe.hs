{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#endif

-- | This module exports a safe subset of the labeled MVar interface.
-- See "LIO.Concurrent.LMVar.TCB" for the documentation.
module LIO.Concurrent.LMVar.Safe ( module LIO.Concurrent.LMVar.TCB ) where

import LIO.Concurrent.LMVar.TCB ( LMVar
                               , labelOfLMVar
                               , newEmptyLMVar, newEmptyLMVarP
                               , newLMVar, newLMVarP
                               , takeLMVar, takeLMVarP
                               , putLMVar, putLMVarP
                               , readLMVar, readLMVarP
                               , swapLMVar, swapLMVarP
                               , tryTakeLMVar, tryTakeLMVarP
                               , tryPutLMVar, tryPutLMVarP
                               , isEmptyLMVar, isEmptyLMVarP
                               , withLMVar, withLMVarP 
                               ) 
