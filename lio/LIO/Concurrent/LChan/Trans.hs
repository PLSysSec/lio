{-# LANGUAGE Safe #-}

{- | 'MonadLIO' generalizations for "LIO.Concurrent.LChan". -}
module LIO.Concurrent.LChan.Trans (
    newLChan
  , newLChanP
  , writeLChan
  , writeLChanP
  , readLChan
  , readLChanP
  , dupLChan
  , dupLChanP
  ) where

import safe LIO.Core
import safe LIO.Label

import safe qualified LIO.Concurrent.LChan as C

-- | See 'LIO.Concurrent.LChan.newLChan'.
newLChan :: (MonadLIO l m, Label l) => l -> m (C.LChan l a)
newLChan = liftLIO . C.newLChan

-- | See 'LIO.Concurrent.LChan.newLChanP'.
newLChanP :: (MonadLIO l m, PrivDesc l p) => Priv p -> l -> m (C.LChan l a)
newLChanP p = liftLIO . C.newLChanP p

-- | See 'LIO.Concurrent.LChan.writeLChan'.
writeLChan :: (MonadLIO l m, Label l) => C.LChan l a -> a -> m ()
writeLChan c = liftLIO . C.writeLChan c

-- | See 'LIO.Concurrent.LChan.writeLChanP'.
writeLChanP :: (MonadLIO l m, PrivDesc l p) => Priv p -> C.LChan l a -> a -> m ()
writeLChanP p c = liftLIO . C.writeLChanP p c

-- | See 'LIO.Concurrent.LChan.readLChan'.
readLChan :: (MonadLIO l m, Label l) => C.LChan l a -> m a
readLChan = liftLIO . C.readLChan

-- | See 'LIO.Concurrent.LChan.readLChanP'.
readLChanP :: (MonadLIO l m, PrivDesc l p) => Priv p -> C.LChan l a -> m a
readLChanP p = liftLIO . C.readLChanP p

-- | See 'LIO.Concurrent.LChan.dupLChan'.
dupLChan :: (MonadLIO l m, Label l) => C.LChan l a -> m (C.LChan l a)
dupLChan = liftLIO . C.dupLChan

-- | See 'LIO.Concurrent.LChan.dupLChanP'.
dupLChanP :: (MonadLIO l m, PrivDesc l p) => Priv p -> C.LChan l a -> m (C.LChan l a)
dupLChanP p = liftLIO . C.dupLChanP p
