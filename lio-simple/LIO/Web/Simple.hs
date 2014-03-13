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
    -- * Utilities for parsing request
    body, parseForm
    -- * Middleware
  , browserLabelGuard
  , removeRequestHeaders
  , removeResponseHeaders 
    -- * LIO applications
  , SimpleLIOApplication, SimpleLIOMiddleware
  , LIOController
    -- * Relevant modules
  , module Web.Simple.Responses
  , module Web.Simple.Templates
  , module Web.Simple.Controller.Trans
  ) where

import safe LIO
import LIO.TCB (ioTCB)
import LIO.Web.Simple.TCB

import safe Web.Simple hiding (request, body, parseForm)
import safe Web.Simple.Responses
import safe Web.Simple.Controller.Trans
import safe Web.Simple.Templates

import Network.Wai.Parse

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import safe qualified Data.ByteString.Lazy.Char8 as L8


-- | Controller with 'LIO' as the underlying monad.
type LIOController l = ControllerT (LIO l)

-- | Parses a HTML form from the request body. It returns a list of 'Param's as
-- well as a list of 'File's, which are pairs mapping the name of a /file/ form
-- field to a 'FileInfo' pointing to a temporary file with the contents of the
-- upload.
--
-- Currently only tursted code can read the file.
parseForm :: Label l
  => LIOController l r ([Param], [(S.ByteString, FileInfo L.ByteString)])
parseForm = do
  req <- request
  liftLIO . ioTCB $ parseRequestBody lbsBackEnd req

-- | Parses a HTML form from the request body. It returns a list of 'Param's as
-- well as a list of 'File's, which are pairs mapping the name of a /file/ form
-- field to a 'FileInfo' pointing to a temporary file with the contents of the

-- | Reads and returns the body of the HTTP request.
--
-- Note: @body@ function consumes the body from a Source IO
-- Bytestring.  Since the 'Request' constructor is exposed by
-- "Network.Wai.Internal", it's important to disallow construction of
-- such values when considering untrusted code.
body :: Label l => LIOController l r L8.ByteString
body = do
  req <- request
  liftLIO . ioTCB $ L8.fromChunks `fmap` (requestBody req $$ CL.consume)

instance Label l => MonadLIO l (ControllerT (LIO l) r) where
  liftLIO act = ControllerT $ \st -> 
      liftLIO act >>= \r -> return (Right r, st)
