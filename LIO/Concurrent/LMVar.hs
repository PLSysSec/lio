{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
-- |This module implements labeled @MVar@s.  The interface is analogous
-- to "Control.Concurrent.MVar", but the operations take place in
-- the LIO monad.  Moreover, taking and putting @MVars@ calls a write
-- guard @wguard@ as every read implies a write and vice versa. This
-- module exports only the safe subset (non-TCB) of the @LMVar@ module
-- trusted code can import "LIO.Concurrent.LMVar.TCB".
-- The interface is documented in "LIO.Concurrent.LMVar.TCB".
module LIO.Concurrent.LMVar ( module LIO.Concurrent.LMVar.Safe ) where

import LIO.Concurrent.LMVar.Safe
