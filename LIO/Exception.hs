{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- | 

This module exports two exception types that the @lio@ library throws
before an IFC violation can take place. 'MonitorFailure' should be
used when the reason for failure is sufficiently described by the
type. Otherwise, 'VMonitorFailure' (i.e., \"Verbose\"-'MonitorFailure')
should be used to further describe the error.

-}

module LIO.Exception (
    MonitorFailure(..)
  , VMonitorFailure(..)
  ) where

import           Data.Typeable
import           Control.Exception

-- | Exceptions thrown when some IFC restriction is about to be
-- violated.
data MonitorFailure = ClearanceViolation
                    -- ^ Current label would exceed clearance, or
                    -- object label is above clearance.
                    | CurrentLabelViolation
                    -- ^ Clearance would be below current label, or
                    -- object label is not above current label.
                    | InsufficientPrivs
                    -- ^ Insufficient privileges. Thrown when lowering
                    -- the current label or raising the clearance
                    -- cannot be accomplished with the supplied
                    -- privileges.
                    | CanFlowToViolation
                    -- ^ Generic can-flow-to failure, use with
                    -- 'VMonitorFailure'
                    deriving (Show, Typeable)

instance Exception MonitorFailure

-- | Verbose version of 'MonitorFailure' also carrying around a
-- detailed message.
data VMonitorFailure = VMonitorFailure { monitorFailure :: MonitorFailure
                                       , monitorMessage :: String }
                    deriving Typeable

instance Show VMonitorFailure where
  show m = (show $ monitorFailure m) ++ ": " ++ (monitorMessage m)

instance Exception VMonitorFailure
