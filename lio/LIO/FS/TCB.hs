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

-}
module LIO.FS.TCB (
  -- * Initializing labeled filesystem
    initFSTCB, mkFSTCB, setFSTCB
  , getRootDirTCB
  -- * Handling path labels
  , setPathLabelTCB
  , getPathLabelTCB
  -- * Creating labeled objects
  , createFileTCB
  , createDirectoryTCB
  -- * Labeled 'FilePath'
  , LFilePath(..)
  -- * Filesystem errors
  , FSError(..)
  -- * Serializable label constraint
  , SLabel
  , lazyEncodeLabel, encodeLabel, decodeLabel
  ) where

import           Data.Serialize
import           Data.Typeable
import           Data.IORef
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Digest.Pure.SHA as SHA

import           Codec.Compression.Zlib hiding (compress)

import           Control.Monad
import           Control.Exception
                 
import           System.FilePath
import           System.Directory
import           System.IO
import           System.IO.Unsafe
import           System.Xattr

import           LIO.Label
import           LIO.Core
import           LIO.TCB

-- | Synonym for strict ByteString
type S8 = S8.ByteString
 
-- | Synonym for lazy ByteString
type L8 = L8.ByteString

-- | Constraintfor serializable labels
type SLabel l = (Label l, Serialize l)

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
getRootDirTCB :: SLabel l => LIO l FilePath
getRootDirTCB = ioTCB $ readIORef rootDir

-- | Create a the file store (i.e., labeled file system) with a given
-- label and root file path.  The path must be an absolute path,
-- otherwise @initFSTCB@ throws 'FSRootInvalid'.
mkFSTCB :: SLabel l
        => FilePath      -- ^ Path to the filesystem root
        -> l             -- ^ Label of root
        -> LIO l ()
mkFSTCB path l = do
  unless (isAbsolute path) $ ioTCB $ throwIO FSRootInvalid
  -- Create root of filesystem:
  ioTCB $ createDirectory path
  -- Set root label:
  setPathLabelTCB path l
  -- Create magic attribute:
  ioTCB $ lsetxattr path magicAttr magicContent CreateMode
  -- Set the root filesystem:
  ioTCB $ writeIORef rootDir path

-- | Set the given file path as the root of the labeled filesystem.  This
-- function throws a 'FSLabelCorrupt' if the directory does not contain a
-- valid label, and  a 'FSRootCorrupt' if the 'magicAttr' attribute is
-- missing.
setFSTCB :: SLabel l => FilePath -> LIO l ()
setFSTCB path = do
  -- Path must be absolute
  unless (isAbsolute path) $ ioTCB $ throwIO FSRootInvalid
  -- Path must be a directory
  checkDirExists
  -- Check magic attribute:
  checkMagic
  -- Check that the label of the root is valid
  void $ getPathLabelTCB path
  -- Set the root directory
  ioTCB $ writeIORef rootDir path
   where checkMagic = ioTCB $ do
           magicOK <-(==magicContent) `liftM` 
                      (throwOnFail $ lgetxattr path magicAttr)
           unless magicOK doFail
         checkDirExists = ioTCB $ do
          e <- doesDirectoryExist path
          unless e $ throwIO FSRootNoExist
         doFail = throwIO FSRootCorrupt
         throwOnFail act = act `catch` (\(_:: SomeException) -> doFail)

-- | Initialize filesystem at the given path. The supplied path must be
-- absolute, otherwise @initFSTCB@ throw 'FSRootInvalid'.  If the FS has
-- already been created then @initFSTCB@ solely verifies that the root
-- directory is not corrupt (see 'setFSTCB'). Otherwise, a new FS is created
-- with the supplied label (see 'mkFSTCB').
--
-- This function performs several checks that 'setFSTCB' and 'mkFSTCB' perform,
-- so when considering performance they should be called directly.
initFSTCB :: SLabel l => FilePath -> Maybe l -> LIO l ()
initFSTCB path ml = do
 unless (isAbsolute path) $ ioTCB $ throwIO FSRootInvalid
 exists <- ioTCB $ doesDirectoryExist path
 (if exists then setFSTCB else mkFSTCB') path
  where mkFSTCB' p = maybe (ioTCB $ throwIO FSRootNeedLabel) (mkFSTCB p) ml
                    

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
lazyEncodeLabel :: SLabel l => l -> L8
lazyEncodeLabel = compress . encodeLazy

-- | Encode a label into an attribute value.
encodeLabel :: SLabel l => l -> AttrValue
encodeLabel = strictify . lazyEncodeLabel

-- | Descode label from an attribute value.
decodeLabel :: SLabel l => AttrValue -> Either String l
decodeLabel = decodeLazy . decompress . lazyfy

-- | Set the label of a given path. This function sets the 'labelAttr'
-- attribute to the encoded label, and the hash to 'labelHashAttr'.
setPathLabelTCB :: SLabel l => FilePath -> l -> LIO l ()
setPathLabelTCB path l = ioTCB $ do
  lsetxattr path labelAttr     (strictify lEnc) RegularMode
  lsetxattr path labelHashAttr lHsh             RegularMode
    where lEnc = lazyEncodeLabel l
          lHsh = strictify . SHA.bytestringDigest . SHA.sha1 $ lEnc

-- | Get the label of a given path. If the object does not have an
-- associated label or the hash of the label and stored-hash are not
-- equal, this function throws 'FSLabelCorrupt'.
getPathLabelTCB :: SLabel l => FilePath -> LIO l l
getPathLabelTCB path = rethrowIoTCB $ do
  (b, h) <- throwOnFail $ do b <- lgetxattr path labelAttr
                             h <- lgetxattr path labelHashAttr
                             return (b, h)
  let b' = lazyfy b
      h' = strictify . SHA.bytestringDigest . SHA.sha1 $ b'
  case decodeLabel b of
    Right l | h == h' -> return l
    _                 -> doFail
  where doFail = throwIO $ FSLabelCorrupt path
        throwOnFail act = act `catch` (\(_:: SomeException) -> doFail)


-- | Create a directory object with the given label.
createDirectoryTCB :: (SLabel l) => l -> FilePath -> LIO l ()
createDirectoryTCB l path = do
  rethrowIoTCB $ createDirectory path
  setPathLabelTCB path l

-- | Create a file object with the given label and return a handle to
-- the new file.
createFileTCB :: (SLabel l) => l -> FilePath -> IOMode -> LIO l Handle
createFileTCB l path mode = do
  h <- rethrowIoTCB $ openFile path mode
  setPathLabelTCB path l
  return h

--
-- Labeled 'FilePath's
--

data LFilePath l = LFilePathTCB { labelOfFilePath :: l
                                -- ^ Label of file path
                                , unlabelFilePathTCB :: FilePath
                                -- ^ Unlabel a filepath, ignoring IFC.
                                } 

--
-- Misc helper
--

-- | Convert lazy ByteString to strict ByteString.
strictify :: L8 -> S8
strictify = S8.concat . L.toChunks

-- | Convert strict ByteString to lazy ByteString.
lazyfy :: S8 -> L8
lazyfy x = L8.fromChunks [x]

-- | Compress with zlib (optimized for speed).
compress :: L8 -> L8
compress = compressWith (defaultCompressParams { compressLevel = bestSpeed })
