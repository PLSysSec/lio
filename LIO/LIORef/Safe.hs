-- |This module exports the safe subset of the "LIO.LIORef.TCB" module.
-- It is important that untrusted code be limited to this subset; information
-- flow can easily be violated if the TCB functions are exported.
module LIO.LIORef.Safe ( module LIO.LIORef.TCB ) where
import LIO.LIORef.TCB hiding ( newLIORefTCB
                             , readLIORefTCB
                             , writeLIORefTCB
                             , atomicModifyLIORefTCB
                             )
