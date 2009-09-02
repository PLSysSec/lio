-- |This module implements labeled IORefs.  The interface is analogous
-- to "Data.IORef", but the operations take place in the LIO monad.
-- Moreover, reading the LIORef calls taint, while writing it calls
-- lguard.
module LIO.LIORef (LIORef
                  , newLIORef, labelOfLIORef
                  , readLIORef, writeLIORef, atomicModifyLIORef
                  ) where

import LIO.TCB
import Data.IORef


data LIORef l a = LIORefTCB l (IORef a)

newLIORef :: (Label l) => l -> a -> LIO l s (LIORef l a)
newLIORef l a = do
  lguard l
  ior <- ioTCB $ newIORef a
  return $ LIORefTCB l ior

labelOfLIORef :: (Label l) => LIORef l a -> l
labelOfLIORef (LIORefTCB l _) = l

readLIORef :: (Label l) => LIORef l a -> LIO l s a
readLIORef (LIORefTCB l r) = do
  taint l
  val <- ioTCB $ readIORef r
  return val

writeLIORef :: (Label l) => LIORef l a -> a -> LIO l s ()
writeLIORef (LIORefTCB l r) a = do
  lguard l
  ioTCB $ writeIORef r a

atomicModifyLIORef :: (Label l) =>
                      LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORef (LIORefTCB l r) f = do
  lguard l
  ioTCB $ atomicModifyIORef r f

