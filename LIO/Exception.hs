{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LIO.Exception (
  -- * LIO exceptions
    LabeledException
  -- * Throw and catch
  , throwLIO
  , catchLIOP, catchLIO
  -- * LIO Monad
  , tryLIO
  -- * Exceptions thrown by LIO
  , MonitorFailure(..), VMonitorFailure(..)
  ) where

import           LIO.Label
import           LIO.Monad
import           LIO.Privs
import           LIO.Guards
import           LIO.Exception.Throw
import           LIO.Exception.MonitorFailure
import           LIO.Exception.TCB 
import           Control.Exception
import           Control.Monad
import           Control.Monad.Loc

--
-- Exceptions
--


-- | Catches an exception, so long as the label at the point where the
-- exception was thrown can flow to the label at which @catchLIOP@ is
-- invoked, modulo the privileges specified.  Note that the handler
-- raises the current label to the joint of the current label and
-- exception label.
catchLIOP :: (Exception e, Priv l p)
          => p
          -> LIO l a
          -> (e -> LIO l a)
          -> LIO l a
catchLIOP p act handler = do 
  clr <- getClearance
  act `catchTCB` \se@(LabeledExceptionTCB l _ seInner) -> 
    case fromException seInner of
     Just e | l `canFlowTo` clr -> taintP p l >> handler e
     _                          -> unlabeledThrowTCB se

-- | Same as 'catchLIOP' but does not use privileges when \"tainting\"
-- by exception label.
catchLIO :: (Exception e, Label l)
         => LIO l a
         -> (e -> LIO l a)
         -> LIO l a
catchLIO = catchLIOP NoPrivs 

-- | Similar to 'evalLIO', but catches any exceptions thrown by
-- untrusted code instead of propagating them.
tryLIO :: Label l
       => LIO l a
        -- ^ LIO computation that may throw an exception
       -> LIOState l
        -- ^ Initial state
       -> IO (Either (LabeledException l) a, LIOState l)
tryLIO act = runLIO (Right `liftM` act `catchTCB` (return . Left))
