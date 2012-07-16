{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- | 

This module exports two exception types that the @lio@ library throws
before an IFC violation can take place.

-}

module LIO.Exception.MonitorFailure ( MonitorFailure(..)
                                    , VMonitorFailure(..)
                                    ) where

import           Data.Typeable
import           Control.Exception

-- | Exceptions thrown by LIO when some IFC restriction is about to be
-- violated.
data MonitorFailure = ClearanceViolation
                    -- ^ Current label would exceed clearance
                    | CurrentLabelViolation
                    -- ^ Clearance would be below current label
                    | InsufficientPrivs
                    -- ^ Insufficient privileges
                    | CanFlowToViolation
                    -- ^ Generic can-flow-to failure
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
