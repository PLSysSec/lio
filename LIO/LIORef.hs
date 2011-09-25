{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#else
#warning "This module is not using SafeHaskell"
#endif
-- |This module implements labeled IORefs.  The interface is analogous
-- to "Data.IORef", but the operations take place in the LIO monad.
-- Moreover, reading the LIORef calls taint, while writing it calls
-- wguard. This module exports only the safe subset (non TCB) of the
-- "LIORef" module -- trusted code can import "LIO.LIORef.TCB".
module LIO.LIORef ( module LIO.LIORef.Safe) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
import safe LIO.LIORef.Safe
#else
import LIO.LIORef.Safe
#endif
