{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |

This module exports the basic interface for creating and using the
labeled file system, implemented as a file store. Trusted code should
use 'initFSTCB' to set the root of the labeled file system. Moreover,
trusted code should implement all the IO functions in terms of
'createFileTCB', 'createDirectoryTCB', and 'getPathLabelTCB' and
'setPathLabelTCB'.

The current implementation uses the 'Show' and 'Read' instance to
serialize and de-serialize labels, respectively. While this is
inefficient, it make it easy to use tools like /getfattr/ to inspect
the labels of files. In a future version we may modify this
implementation to use binary encoding and/or compression (since
filesystem extended attributes are large, but limited).

-}
module LIO.FS.TCB (
  -- * Initializing labeled filesystem
    initFSTCB, mkFSTCB, setFSTCB
  , getRootDirTCB
  -- * Handling path labels
  , setPathLabelTCB
  , getPathLabelTCB
  -- * Helpers for creating labeled objects
  , createFileTCB, createBinaryFileTCB
  , createDirectoryTCB
  -- * Filesystem errors
  , FSError(..)
  ) where

import safe Data.Maybe (listToMaybe)
import safe Data.Typeable
import safe Data.IORef
import safe qualified Data.ByteString.Char8 as S8
import safe qualified Data.ByteString as S
import safe qualified Data.ByteString.Lazy.Char8 as L8
import safe qualified Data.Digest.Pure.SHA as SHA

import safe Control.Monad
import safe Control.Exception
import safe qualified Control.Exception as E
                 
import safe System.FilePath
import safe System.Directory
import safe System.IO
import System.IO.Unsafe
import safe System.Xattr

import safe LIO
import safe LIO.Error
import LIO.TCB
 
--
-- Exception thrown by the file store interface
--

-- | Filesystem errors
data FSError = FSRootCorrupt           -- ^ Root structure is corrupt.
             | FSRootInvalid           -- ^ Root is invalid (must be absolute).
             | FSRootExists            -- ^ Root already exists.
             | FSRootNoExist           -- ^ Root does not exists.
             | FSRootNeedLabel         -- ^ Cannot create root, missing label.
             | FSObjNeedLabel          -- ^ FSobjectcannot be created without a label.
             | FSLabelCorrupt FilePath -- ^ Object label is corrupt.
             | FSIllegalFileName       -- ^ Supplied file name is illegal.
      deriving Typeable

instance Exception FSError

instance Show FSError where
  show FSRootCorrupt      = "Root structure is corrupt."
  show FSRootInvalid      = "Root path is invalid, must be absolute."
  show FSRootExists       = "Root already exists."
  show FSRootNoExist      = "Root directory does not exist."
  show FSRootNeedLabel    = "Root cannot be created without a label."
  show (FSLabelCorrupt f) = "Label of " ++ show f ++ " is corrupt/non-existant."
  show FSObjNeedLabel     = "FS object cannot be created without a label."
  show FSIllegalFileName  = "Supplied file name is illegal."

--
-- Handling root of FS
--

magicAttr :: AttrName
magicAttr = "user._lio_magic"

-- | Content written to magic key. 
magicContent :: AttrValue
magicContent = S.pack  [ 0x7f, 0x45, 0x4c, 0x46, 0x01
                       , 0x01, 0x01, 0x00, 0x00, 0x00
                       , 0x00, 0x00, 0x00, 0x00, 0x00
                       , 0x00, 0xde, 0xad, 0xbe, 0xef]

-- | Root of labeled filesystem.
rootDir :: IORef FilePath
{-# NOINLINE rootDir #-}
rootDir = unsafePerformIO $ newIORef (error "LIO Filesystem not initialized.")

-- | Get the root directory.
getRootDirTCB :: Label l => LIO l FilePath
getRootDirTCB = withContext "getRootDirTCB" $ ioTCB $ readIORef rootDir

-- | Create a the file store (i.e., labeled file system) with a given
-- label and root file path.  The path must be an absolute path,
-- otherwise @initFSTCB@ throws 'FSRootInvalid'.
mkFSTCB :: Label l
        => FilePath      -- ^ Path to the filesystem root
        -> l             -- ^ Label of root
        -> LIO l ()
mkFSTCB path l = withContext "mkFSTCB" $ ioTCB $ do
  unless (isAbsolute path) $ throwIO FSRootInvalid
  -- Create root of filesystem:
  createDirectory path
  -- Set root label:
  setPathLabelTCB path l
  -- Create magic attribute:
  lsetxattr path magicAttr magicContent CreateMode
  -- Set the root filesystem:
  writeIORef rootDir path

-- | Set the given file path as the root of the labeled filesystem.  This
-- function throws a 'FSLabelCorrupt' if the directory does not contain a
-- valid label, and  a 'FSRootCorrupt' if the 'magicAttr' attribute is
-- missing.
setFSTCB :: Label l => FilePath -> LIO l l
setFSTCB path = withContext "setFSTCB" $ ioTCB $ do
  -- Path must be absolute
  unless (isAbsolute path) $ throwIO FSRootInvalid
  -- Path must be a directory
  checkDirExists
  -- Check magic attribute:
  checkMagic
  -- Get the label of the root
  l <- getPathLabelTCB path
  -- Set the root directory
  writeIORef rootDir path
  return l
   where checkMagic = do
           magicOK <-(==magicContent) `liftM` 
                      (throwOnFail $ lgetxattr path magicAttr)
           unless magicOK doFail
         checkDirExists = do
          e <- doesDirectoryExist path
          unless e $ throwIO FSRootNoExist
         doFail = throwIO FSRootCorrupt
         throwOnFail act = act `E.catch` (\(_:: SomeException) -> doFail)

-- | Initialize filesystem at the given path. The supplied path must be
-- absolute, otherwise @initFSTCB@ throw 'FSRootInvalid'.  If the FS has
-- already been created then @initFSTCB@ solely verifies that the root
-- directory is not corrupt (see 'setFSTCB') and returns the label of
-- the root. Otherwise, a new FS is created with the supplied label
-- (see 'mkFSTCB').
--
-- This function performs several checks that 'setFSTCB' and 'mkFSTCB' perform,
-- so when considering performance they should be called directly.
initFSTCB :: Label l => FilePath -> Maybe l -> LIO l l
initFSTCB path ml = withContext "initFSTCB" $ do
 unless (isAbsolute path) $ ioTCB $ throwIO FSRootInvalid
 exists <- ioTCB $ doesDirectoryExist path
 (if exists then setFSTCB else mkFSTCB') path
  where mkFSTCB' f = maybe (throwLIO FSRootNeedLabel) 
                           (\l -> mkFSTCB f l >> return l) ml
                    

--
-- Objects
-- 

-- | Label attribute name.
labelAttr :: AttrName
labelAttr = "user._lio_label"

-- | Hash-of-label attribute name.
labelHashAttr :: AttrName
labelHashAttr = "user._lio_label_sha"

-- | Encode a label into an attribute value.
encodeLabel :: Label l => l -> AttrValue
encodeLabel = S8.pack . show

-- | Descode label from an attribute value.
decodeLabel :: Label l => AttrValue -> Maybe l
decodeLabel = fmap fst . listToMaybe . reads . S8.unpack

-- | Set the label of a given path. This function sets the 'labelAttr'
-- attribute to the encoded label, and the hash to 'labelHashAttr'.
setPathLabelTCB :: Label l => FilePath -> l -> IO ()
setPathLabelTCB path l = do
  lsetxattr path labelAttr     lEnc        RegularMode
  lsetxattr path labelHashAttr (hash lEnc) RegularMode
    where lEnc = encodeLabel l
          hash = L8.toStrict . SHA.bytestringDigest . SHA.sha1 . L8.fromStrict

-- | Get the label of a given path. If the object does not have an
-- associated label or the hash of the label and stored-hash are not
-- equal, this function throws 'FSLabelCorrupt'.
getPathLabelTCB :: Label l => FilePath -> IO l
getPathLabelTCB path = do
  (b, h) <- throwOnFail $ do b <- lgetxattr path labelAttr
                             h <- lgetxattr path labelHashAttr
                             return (b, h)
  let b' = L8.fromStrict b
      h' = L8.toStrict . SHA.bytestringDigest . SHA.sha1 $ b'
  case decodeLabel b of
    Just l | h == h' -> return l
    _                -> doFail
  where doFail = throwIO $ FSLabelCorrupt path
        throwOnFail act = act `E.catch` (\(_:: SomeException) -> doFail)

-- | Create a file object with the given label and return a handle to
-- the new file.
createFileTCB :: Label l => l -> FilePath -> IOMode -> LIO l Handle
createFileTCB l path mode = withContext "createFileTCB" $ ioTCB $ do
  h <- openFile path mode
  setPathLabelTCB path l
  return h

-- | Same as 'createFileTCB' but opens the file in binary mode.
createBinaryFileTCB :: Label l => l -> FilePath -> IOMode -> LIO l Handle
createBinaryFileTCB l path mode = withContext "createBinaryFileTCB" $ioTCB $ do
  h <- openBinaryFile path mode
  setPathLabelTCB path l
  return h

-- | Create a directory object with the given label.
createDirectoryTCB :: Label l => l -> FilePath -> LIO l ()
createDirectoryTCB l path = withContext "createDirectoryTCB" $ ioTCB $ do
  createDirectory path
  setPathLabelTCB path l
