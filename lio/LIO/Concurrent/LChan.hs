{-# LANGUAGE Trustworthy #-}

{- |

Unbounded FIFO channels in the 'LIO' monad. As with other objects in
LIO, a channel has an associated label that is used to impose
restrictions on its operations. In fact, labeled channels ('LChan's)
are simply labeled 'Chan's with read and write access restricted
according to the label. This module is analogous to
"Control.Concurrent.Chan", but the operations take place in the 'LIO'
monad.

-}

module LIO.Concurrent.LChan (
  LChan
  -- * Basic Functions
  -- ** Create labeled 'IORef's
  , newLChan, newLChanP
  -- ** Read 'LChan's
  , readLChan, readLChanP
  -- ** Write 'LChan's
  , writeLChan, writeLChanP
  -- ** Dupicate 'LChan's
  , dupLChan, dupLChanP
  ) where

import safe Control.Concurrent.Chan

import safe LIO.Core
import safe LIO.Error
import safe LIO.Label
import LIO.TCB.LObj

-- | A @LChan@ is a labeled channel, i.e., an unbounded FIFO channel.
type LChan l a = LObj l (Chan a)


-- | Create a new labeled channel.  Note that the supplied label must
-- be above the current label and below the current clearance.  An
-- exception will be thrown by the underlying 'guardAlloc' if this is
-- not the case.
newLChan :: Label l => l -> LIO l (LChan l a)
newLChan l = guardIOTCB (withContext "newLChan" $ guardAlloc l) $
  LObjTCB l `fmap` newChan

-- | Same as 'newLChan' except it takes a set of privileges which are
-- accounted for in comparing the label of the Chan to the current label.
newLChanP :: PrivDesc l p => Priv p -> l -> LIO l (LChan l a)
newLChanP p l = guardIOTCB (withContext "newLChanP" $ guardAllocP p l) $
  LObjTCB l `fmap` newChan

-- | Write value to the labeled channel. The label of the channel must
-- be bounded by the current label and clearance.
writeLChan :: Label l => LChan l a -> a -> LIO l ()
writeLChan = blessWriteOnlyTCB "writeLChan" writeChan

-- | Same as 'writeLChan', but uses privileges when comparing the
-- current label to the label of the channel.
writeLChanP :: PrivDesc l p => Priv p -> LChan l a -> a -> LIO l ()
writeLChanP = blessWriteOnlyPTCB "writeLChanP" writeChan

-- | Read the next value from the channel.  The current label is
-- raised to join of the channel label and current label. Howerver,
-- the label of the channel must be below the current clearance.
readLChan :: Label l => LChan l a -> LIO l a
readLChan = blessReadOnlyTCB "readLChan" readChan

-- | Same as 'readLChan', but takes a privilege object which is used
-- when the current label is raised to avoid over-taining the context.
readLChanP :: PrivDesc l p => Priv p -> LChan l a -> LIO l a
readLChanP = blessReadOnlyPTCB "readLChanP" readChan

-- | Duplicate labeled channel. The label of the channel must be
-- bounded by the current label and clearance.
dupLChan :: Label l => LChan l a -> LIO l (LChan l a)
dupLChan (LObjTCB l ch)= guardIOTCB (withContext "dupLChan" $ guardAlloc l) $
  LObjTCB l `fmap` dupChan ch

-- | Same as 'dupLChan', but uses privileges when comparing the
-- current label to the label of the channel.
dupLChanP :: PrivDesc l p => Priv p -> LChan l a -> LIO l (LChan l a)
dupLChanP p (LObjTCB l ch) = 
  guardIOTCB (withContext "dupLChanP" $ guardAllocP p l) $
    LObjTCB l `fmap` dupChan ch
