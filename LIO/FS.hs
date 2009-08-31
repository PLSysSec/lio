{-# OPTIONS_GHC -XDeriveDataTypeable #-}

{- |This module manages a file store in which a label is associated
    with every file and directory.  The file store is grouped into
    directories by label.  Files are stored under names like:

    > LabelHash/OpaqueName

    where LabelHash is a SHA-224 hash of the label, and OpaqueName is
    either a regular file (containing contents) or a directory
    populated exclusively by symbolic links pointing back into
    LabelHash directories.  Each LabelHash directory also has a file
    called

    > LabelHash/LABEL

    which actually contains the label of all the files in that directory.

    There is also a symbolic link @root@, pointing to the root
    directory.  For efficiency, @LabelHash@ actually consists of
    multiple directories.
 -}

module LIO.FS ( -- * The opaque name object
                Name -- Do not Export constructor!  Names are TRUSTED
              , rootDir, lookupName
              , IOMode
              -- * Initializing the file system
              , mkRoot
              -- * Internal data structures
              , Node
              -- * Helper functions in the IO Monad
              , labelOfName, labelOfNode, nodeOfName
              , mkNodeDir, mkNodeReg, linkNode
              , openNode
              -- * Misc. utility functions
              , tryPred
              ) where

import LIO.Armor
import LIO.TCB
import LIO.TmpFile

import Prelude hiding (catch)

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Typeable
import qualified GHC.IOBase
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error hiding (catch, try)
import System.FilePath
import System.Posix.Directory hiding (removeDirectory)
import System.Posix.Files
import System.Posix.IO

import Data.Digest.Pure.SHA
import qualified System.IO.Cautious as CIO

--
-- Utility functions
--

strictReadFile   :: FilePath -> IO LC.ByteString
strictReadFile f = withFile f ReadMode readit
    where readit h = do
            size <- hFileSize h
            LC.hGet h $ fromInteger size

catchIO     :: IO a -> IO a -> IO a
catchIO a h = catch a ((const :: a -> IOException -> a) h)

catchPred          :: Exception e => (e -> Bool) -> IO a -> IO a -> IO a
catchPred pred a h = catchJust test a runh
    where
      test e = if pred e then Just () else Nothing
      runh () = h

tryPred        :: Exception e => (e -> Bool) -> IO a -> IO (Either e a)
tryPred pred a = tryJust test a
    where
      test e = if pred e then Just e else Nothing

ignoreErr :: IO () -> IO ()
ignoreErr m = catch m ((\e -> return ()) :: SomeException -> IO ())

-- |Delete a name whether it's a file or directory, by trying both.
-- This is slow, but only used for error conditions when performance
-- shouldn't matter.
clean :: FilePath -> IO ()
clean path =
    removeFile path `catchIO` (removeDirectory path `catchIO` return ())

--
-- Exceptions thrown by this module
--

data FSErr
    = FSCorruptLabel FilePath   -- ^File Containing Label is Corrupt
      deriving (Show, Typeable)
instance Exception FSErr

--
-- LDir functions
--

prefix = "ls"

-- |File name in which labels are stored in 'LDir's.
labelFile :: FilePath
labelFile = "LABEL"

-- |Type containing the pathname of a @LabelHash@ directory (which
-- must contain a file named 'labelFile').
newtype LDir = LDir FilePath deriving (Show)

-- |Hash a label down to the directory storing all 'Node's with that
-- label.
lDirOfLabel   :: (Label l) => l -> LDir
lDirOfLabel l =
    case armor32 $ bytestringDigest $ sha224 $ LC.pack $ show l of
      c1:c2:c3:rest -> LDir (prefix </> (c1:[]) </> (c2:[])
                                        </> (c3:[]) </> rest)

-- |'LDir' that contains a 'Node'
lDirOfNode          :: Node -> LDir
lDirOfNode (Node n) = LDir $ takeDirectory n

-- |'LDir' that contains the directory that contains a file name.
lDirOfName          :: Name -> LDir
lDirOfName (NameTCB n) = LDir $ takeDirectory $ takeDirectory n

-- |Takes an LDir and returns the label stored in 'labelFile' in that
-- directory.  May throw 'FSCorruptLabel'.
labelOfLDir          :: (Label l) => LDir -> IO l
labelOfLDir (LDir p) = do
  s <- strictReadFile target `catch` diagnose
  parseit s
    where
      target = (p </> labelFile)
      parseit s = case reads $ LC.unpack s of
                    (l, "\n"):_ -> return l
                    _ -> throwIO $ FSCorruptLabel target
      diagnose e | isDoesNotExistError e = do
                         exists <- doesDirectoryExist p
                         if exists
                           then throwIO $ FSCorruptLabel target
                           else throwIO e
                 | otherwise             = throwIO e

-- |Gets the LDir for a particular label.  Creates it if it does not
-- exist.  May throw 'FSCorruptLabel'.
getLDir   :: Label l => l -> IO LDir
getLDir l = try (labelOfLDir ldir) >>= handle
    where
      ldir@(LDir dir) = lDirOfLabel l
      handle (Right l')
          | l' == l   = return ldir
          | otherwise = dumplabel >> throwIO (FSCorruptLabel dir)
      handle (Left e) =
          case fromException e of
            Just e' | isDoesNotExistError e' -> makedir
            _                                -> dumplabel >> throwIO e
      makelabel path = CIO.writeFile path $ shows l "\n"
      makedir = do
        let tdir = dir ++ newNodeExt
        createDirectoryIfMissing True tdir
        makelabel $ tdir </> labelFile
        rename tdir dir
        return ldir
      dumplabel = ignoreErr $ makelabel $ dir </> (labelFile ++ ".correct")

--
-- Node functions
--

-- |The @Node@ type represents filenames of the form
-- @LabelHash\/OpaqueName@.  These names must always point to regular
-- files or directories (not symbolic links).  There must always exist
-- a file @LabalHash\/.label@ specifying the label of a @Node@.
newtype Node = Node FilePath deriving (Show)

-- |When a @Node@ is first created, it has a file name with a \'~\'
-- character at the end.  This is so that in the case of a crash, a
-- node that was not linked to can be easily recognized and deleted.
-- The @NewNode@ type wrapper represents a node that is not yet linked
-- to.
newtype NewNode = NewNode Node deriving (Show)

-- |String that gets appended to new file names.  After a crash these
-- may need to be garbage collected.
newNodeExt :: String
newNodeExt = "~"

-- |Label protecting the contents of a node.
labelOfNode :: (Label l) => Node -> IO l
labelOfNode = labelOfLDir . lDirOfNode

-- | Create new Node in the appropriate directory for a given label.
-- The node gets created with an extra ~ appended.
mkNode     :: (Label l) => l
           -- ^Label for the new node
           -> (FilePath -> String -> IO (a, FilePath))
           -- ^Either 'mkTmpDir' or 'mkTmpFile' with curried 'IOMode'
           -> IO (a, NewNode)
           -- ^Returns file handle or () and destination path
mkNode l f = do
  (LDir d) <- getLDir l
  (a, p) <- f d newNodeExt
  let p' = init p
  exists <- catchIO (getFileStatus p' >> return True) (return False)
  if not exists
    then return (a, NewNode $ Node p')
    else do
      hPutStrLn stderr $ "mkNode: file " ++ p' ++ " already exists." -- XXX
      clean p
      mkNode l f

-- |Wrapper around mkNode to create a directory.
mkNodeDir   :: (Label l) => l -> IO NewNode
mkNodeDir l = liftM snd (mkNode l mkTmpDir)

-- |Wrapper around mkNode to create a regular file.
mkNodeReg     :: (Label l) => IOMode -> l -> IO (Handle, NewNode)
mkNodeReg m l = mkNode l (mkTmpFile m)

-- | Used when creating a symbolic link named @src@ that points to
-- @dst@.  If both @src@ and @dst@ are relative to the current working
-- directory and in subdirectories, then the contents of the symbolic
-- link cannot just be @dst@, instead it is @makeRelativeTo dst src@.
makeRelativeTo          :: FilePath -- ^Destination of symbolic link
                        -> FilePath -- ^Name of symbolic link
                        -> FilePath -- ^Returns contents to put in symbolic link
makeRelativeTo dest src =
    doit (splitDirectories dest) (init $ splitDirectories src)
    where
      doit [] []                      = "."
      doit (d1:ds) (s1:ss) | d1 == s1 = doit ds ss
      doit d s = joinPath (replicate (length s) ('.':'.':pathSeparator:[]) ++ d)

-- |Assign a 'Name' to a 'NewNode', turning it into a 'Node'.  Note
-- that unlike the Unix file system, only a single link may be created
-- to each node.
linkNode                                      :: NewNode -> Name -> IO Node
linkNode (NewNode (Node path)) (NameTCB name) = do
  let tpath = path ++ newNodeExt
  createSymbolicLink (path `makeRelativeTo` name) name `onException` clean tpath
  rename tpath path `onException` removeFile name
  return $ Node path

openNode                  :: Node -> IOMode -> IO Handle
openNode (Node file) mode = openFile file mode

--
-- Name functions
--
  
-- |The @Name@ type represents user-chosen (non-opaque) filenames of
-- symbolic links, either @\"root\"@ or pathnames of the form
-- @LabelHash\/OpaqueName\/filename@.  Intermediary components of the
-- file name must not be symbolic links.
newtype Name = NameTCB FilePath deriving (Show)

-- |Name of root directory.
rootDir :: Name
rootDir = NameTCB $ prefix </> "root"

-- |Label protecting the name of a file.  Note that this is the label
-- of the directory containing the file name, not the label of the
-- Node that the file name designates.
labelOfName :: (Label l) => Name -> IO l
labelOfName = labelOfLDir . lDirOfName

unlinkName                  :: (FilePath -> IO ()) -> Name -> IO ()
unlinkName f (NameTCB name) = do
  (Node node) <- nodeOfName (NameTCB name)
  f node
  removeFile name

-- |Remove a directory by name.
unlinkNameDir = unlinkName removeDirectory

-- |Remove a regular file by name.
unlinkNameReg = unlinkName removeFile
  
-- |This function reads the contents of a symbolic link and returns
-- the pathname of its destination, relative to the current working
-- directory.  It elides ".." components at the begining of the
-- symbolic link contents, so that if the link @foo\/bar -> ..\/baz@
-- exists, @expandLink \"foo\/bar\"@ will return @\"foo\/baz\"@.
--
-- /Warning:/ This function assumes no itermediary components of the
-- path to the symbolic link are also symbolic links.
expandLink      :: FilePath -> IO FilePath
expandLink path = do
  suffix <- catchPred (\e -> ioeGetErrorType e == GHC.IOBase.InvalidArgument)
            (readSymbolicLink path) (return "")
  return $ if (isAbsolute suffix)
           then suffix
           else domerge (takeDirectory path) suffix
    where
      domerge [] suffix = suffix
      domerge path [] = path
      domerge path ('.':'.':pathSeparator:suffix) =
          domerge (takeDirectory path) suffix
      domerge path suffix = path </> suffix

-- |'Node' that a 'Name' is pointing to.
nodeOfName             :: Name -> IO Node
nodeOfName (NameTCB n) = liftM Node $ expandLink n

-- |Gives the 'Name' of a directory entry in a directory 'Node'.
nodeEntry                  :: Node -> FilePath -> Name
nodeEntry (Node node) name = NameTCB (node </> name)

mkRoot   :: (Label l) => l -> IO ()
mkRoot l = do
  let (NameTCB root) = rootDir
  exists <- doesDirectoryExist root
  when exists $ throwIO $ mkIOError alreadyExistsErrorType
           "root directory already exists" Nothing (Just root)
  new <- mkNodeDir l
  linkNode new rootDir
  return ()

--
-- LIO Monad function
--

-- |Looks up a FilePath, turning it into a 'Name', and raising to
-- current label to reflect all directories traversed.  Note that this
-- only looks up a 'Name'; it does not ensure the 'Name' actually
-- exists.  The intent is that you call lookupName before creating or
-- opening files.
--
-- Note that this function will touch bad parts of the file system if
-- it is supplied with a malicous 'Name'.  Thus, it is important to
-- keep the constructur of 'Name' private, so that the only way for
-- user code to generate names is to start with 'rootDir' and call
-- @lookupName@.
lookupName                 :: (Priv l p) =>
                              p    -- ^Privileges to limit tainting
                           -> Name -- ^Start point (e.g., rootDir)
                           -> FilePath -- ^Name to look up
                           -> LIO l s Name
lookupName priv start path = 
    dolookup start (stripslash $ splitDirectories path)
    where
      stripslash ((c:_):t) | c == pathSeparator = t
      stripslash t = t
      dolookup name [] = return name
      dolookup name (".":rest) = dolookup name rest
      dolookup _ ("..":_) = throwL $ mkIOError doesNotExistErrorType
                            "illegal filename" Nothing (Just ".." )
      dolookup name (cn:rest) = do
        node <- ioTCB $ nodeOfName name -- Shouldn't fail
        label <- rtioTCB $ labelOfNode node -- Can fail if no such file
        ptaintio priv label
        dolookup (nodeEntry node cn) rest

