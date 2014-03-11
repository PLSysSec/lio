{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
 
 This module defines several instances needed to use 'LIO' as the
 underlying monad with "Web.Simple", the simple web framework.
 Additionally, we provide some middleware for executing apps in a safe
 manner.

 "LIO.Web.Simple.TCB" defines several functions that can be used to
 execute 'LIO' web applications with the Warp server.

-}
module LIO.Web.Simple (
    -- * Middleware
    browserLabelGuard
  , removeRequestHeaders
  , removeResponseHeaders 
    -- * LIO applications
  , SimpleLIOApplication, SimpleLIOMiddleware
  ) where

import safe LIO
import LIO.TCB (ioTCB)
import LIO.Web.Simple.TCB

import safe Web.Simple

import Network.Wai.Parse

import Data.Conduit
import qualified Data.Conduit.List as CL

import safe qualified Data.ByteString.Lazy.Char8 as L8

-- | Provide an instance for MonadController in the LIO monad. Note
-- that th 'body' implementation consumes the body from a Source IO
-- Bytestring. Since the 'Request' constructor is exposed by
-- "Network.Wai.Internal", it's important to disallow construction of
-- such values when considering untrusted code.
instance Label l => MonadController (LIO l) where
  parseForm = do
    req <- request
    liftLIO . ioTCB $ parseRequestBody lbsBackEnd req
  body = do
    req <- request
    liftLIO . ioTCB $ L8.fromChunks `fmap` (requestBody req $$ CL.consume)
  liftController = liftLIO

instance Label l => MonadLIO l (ControllerM r (LIO l)) where
  liftLIO act = ControllerM $ \st -> 
      liftLIO act >>= \r -> return (Right r, st)
