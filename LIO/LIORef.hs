{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
-- |This module implements labeled IORefs.  The interface is analogous
-- to "Data.IORef", but the operations take place in the LIO monad.
-- (See "LIO.LIORef.TCB" for documentation.)
-- Moreover, reading the LIORef calls taint, while writing it calls
-- aguard. This module exports only the safe subset (non TCB) of the
-- @LIORef@ module -- trusted code can import "LIO.LIORef.TCB".
module LIO.LIORef ( module LIO.LIORef.Safe) where

import LIO.LIORef.Safe
