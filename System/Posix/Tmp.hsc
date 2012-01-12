{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Tmp
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
--                    Deian Stefan <deian@cs.stanford.edu>
-- This is a copy of the /latest/ @System.Posix.Temp@ module. Because
-- @Temp@ will not be made available until GHC 7.6, we are including
-- the core functions here.
--
-----------------------------------------------------------------------------

module System.Posix.Tmp (
        mkstemp, mkstemps, mkdtemp
    ) where

#include "HsTmp.h"

import Foreign.C
import System.IO
import System.Posix.IO
import System.Posix.Types

#if __GLASGOW_HASKELL__ > 700
import System.Posix.Internals (withFilePath, peekFilePath)

#elif __GLASGOW_HASKELL__ > 611
import System.Posix.Internals (withFilePath)

peekFilePath :: CString -> IO FilePath
peekFilePath = peekCString

#else
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = withCString

peekFilePath :: CString -> IO FilePath
peekFilePath = peekCString
#endif

#if HAVE_MKSTEMP
foreign import ccall unsafe "HsUnix.h __hscore_mkstemp"
  c_mkstemp :: CString -> IO CInt
#endif

-- | Make a unique filename and open it for reading\/writing. The returned
-- 'FilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
mkstemp :: String -> IO (FilePath, Handle)
#if HAVE_MKSTEMP
mkstemp template' = do
  let template = template' ++ "XXXXXX"
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
mkstemp = error "System.Posix.Temp.mkstemp: not supported"
#endif

#if HAVE_MKSTEMPS
foreign import ccall unsafe "HsUnix.h __hscore_mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt
#endif

-- | Make a unique filename with a given prefix and suffix and open it for
-- reading\/writing. The returned 'FilePath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between the prefix
-- and suffix. The first argument is the desired prefix of the filepath of the
-- temporary file to be created. The second argument is the suffix of the
-- temporary file to be created.
--
-- If you are using as system that doesn't support the mkstemps glibc function
-- (supported in glibc > 2.11) then this function simply throws an error.
mkstemps :: String -> String -> IO (FilePath, Handle)
#if HAVE_MKSTEMPS
mkstemps prefix suffix = do
  let template = prefix ++ "XXXXXX" ++ suffix
      lenOfsuf = (fromIntegral $ length suffix) :: CInt
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemps" (c_mkstemps ptr lenOfsuf)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
mkstemps = error "System.Posix.Temp.mkstemps: not supported"
#endif

#if HAVE_MKDTEMP
foreign import ccall unsafe "HsUnix.h __hscore_mkdtemp"
  c_mkdtemp :: CString -> IO CString
#endif

-- | Make a unique directory. The returned 'FilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
mkdtemp :: String -> IO FilePath
mkdtemp template' = do
#if HAVE_MKDTEMP
  let template = template' ++ "XXXXXX"
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name
#else
mkdtemp = error "System.Posix.Temp.mkdtemp: not supported"
#endif
