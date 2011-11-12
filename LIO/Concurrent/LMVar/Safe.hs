{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#else
#warning "This module is not using SafeHaskell"
#endif

module LIO.Concurrent.LMVar.Safe ( module LIO.Concurrent.LMVar.TCB ) where

import LIO.Concurrent.LMVar.TCB ( LMVar
                               , newEmptyLMVar, newEmptyLMVarP
                               , newLMVar, newLMVarP
                               , takeLMVar, takeLMVarP
                               , putLMVar, putLMVarP
                               , readLMVar, readLMVarP
                               , labelOfLMVar
                               ) 
