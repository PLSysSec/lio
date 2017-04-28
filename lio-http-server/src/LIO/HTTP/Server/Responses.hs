{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- Taken from Simple 
-- | This module defines some convenience functions for creating responses.
module LIO.HTTP.Server.Responses
  ( ok, okHtml, okJson, okXml
  , movedTo, redirectTo
  , badRequest, requireBasicAuth, forbidden
  , notFound
  , serverError
  ) where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import LIO.HTTP.Server


-- | Type alias for 'S8.ByteString'
type ContentType = S8.ByteString

-- | Creates a 200 (OK) 'Response' with the given content-type and resposne
-- body
ok :: ContentType -> L8.ByteString -> Response
ok contentType body =
  Response status200 [(hContentType, contentType)] body

-- | Helper to make responses with content-type \"text/html\"
mkHtmlResponse :: Status -> [Header] -> L8.ByteString -> Response
mkHtmlResponse stat hdrs =
  Response stat ((hContentType, S8.pack "text/html"):hdrs)

-- | Creates a 200 (OK) 'Response' with content-type \"text/html\" and the
-- given resposne body
okHtml :: L8.ByteString -> Response
okHtml body =
  mkHtmlResponse status200 [] body

-- | Creates a 200 (OK) 'Response' with content-type \"application/json\" and the
-- given resposne body
okJson :: L8.ByteString -> Response
okJson = ok (S8.pack "application/json")

-- | Creates a 200 (OK) 'Response' with content-type \"application/xml\" and the
-- given resposne body
okXml :: L8.ByteString -> Response
okXml = ok (S8.pack "application/xml")

-- | Given a URL returns a 301 (Moved Permanently) 'Response' redirecting to
-- that URL.
movedTo :: String -> Response
movedTo url = mkHtmlResponse status301 [(hLocation, S8.pack url)] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>301 Moved Permanently</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>Moved Permanently</H1>\n\
              \<P>The document has moved <A HREF=\""
             , L8.pack url
             , L8.pack "\">here</A>\n\
                       \</BODY></HTML>\n"]

-- | Given a URL returns a 303 (See Other) 'Response' redirecting to that URL.
redirectTo :: S8.ByteString -> Response
redirectTo url = mkHtmlResponse status303 [(hLocation, url)] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>303 See Other</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>See Other</H1>\n\
              \<P>The document has moved <A HREF=\""
             , L8.fromChunks [url]
             , L8.pack "\">here</A>\n\
                       \</BODY></HTML>\n"]

-- | Returns a 400 (Bad Request) 'Response'.
badRequest :: Response
badRequest = mkHtmlResponse status400 [] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>400 Bad Request</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>Bad Request</H1>\n\
              \<P>Your request could not be understood.</P>\n\
                       \</BODY></HTML>\n"]

-- | Returns a 401 (Authorization Required) 'Response' requiring basic
-- authentication in the given realm.
requireBasicAuth :: String -> Response
requireBasicAuth realm = mkHtmlResponse status401
  [("WWW-Authenticate", S8.concat ["Basic realm=", S8.pack . show $ realm])] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>401 Authorization Required</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>Authorization Required</H1>\n\
                       \</BODY></HTML>\n"]

-- | Returns a 403 (Forbidden) 'Response'.
forbidden :: Response
forbidden = mkHtmlResponse status403 [] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>403 Forbidden</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>Forbidden</H1>\n\
              \<P>You don't have permission to access this page.</P>\n\
                       \</BODY></HTML>\n"]

-- | Returns a 404 (Not Found) 'Response'.
notFound :: Response
notFound = mkHtmlResponse status404 [] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>404 Not Found</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>Not Found</H1>\n\
              \<P>The requested URL was not found on this server.</P>\n\
                       \</BODY></HTML>\n"]

-- | Returns a 500 (Server Error) 'Response'.
serverError :: L8.ByteString -> Response
serverError message = mkHtmlResponse status500 [] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>500 Internal Server Error</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>Internal Server Error</H1>\n\
              \<P>", message,
              "</P></BODY></HTML>\n"]


