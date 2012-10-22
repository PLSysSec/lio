{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances,
             MultiParamTypeClasses #-}
{- | 

This module abstracts the basic file 'Handle' methods provided by the
system library, and provides a 'LabeledHandle' type that can be
manipulated from within the 'LIO' Monad. A 'LabeledHandle' is imply a
file 'Handle' with an associated label that is used to track and
control the information flowing from and to the file. The API exposed
by this module is analogous to "System.IO", and the functions mainly
differ in taking an additional label and enforcing IFC.

The actual storage of labeled files is handled by the "LIO.FS" module.
The filesystem is implemented as a file store in which labels are
associated with files and directories (using extended attributes).

/IMPORTANT:/ To use the labeled filesystem you must use
'evalWithRootFS' (or other initializers from "LIO.FS.TCB"), otherwise
any actions built using the combinators of this module will crash.

An example use case shown below: 

>
>  main = dcEvalWithRoot "/tmp/lioFS" $ do
>    createDirectoryP p lsecrets "secrets"
>    writeFileP p ("secrets" </> "alice" ) "I like Bob!"
>      where p = ...
>            lsecrets = ....
>

The file store for the labeled filesystem (see "LIO.FS") will
be created in @\/tmp\/lioFS@, but this is transparent and the user
can think of the filesystem as having root @/@. Note that for this to
work the filesystem must be mounted with the @user_xattr@ option.
For example, on GNU/Linux:

> mount -o rw,noauto,user,sync,noexec,user_xattr /dev/sdb1 /tmp/lioFS

In the current version of the filesystem, there is no notion of
changeable current working directory in the 'LIO' Monad, nor symbolic
links.
-}
module LIO.Handle ( evalWithRootFS
                  , SLabel
                  , SMonadLIO
                    -- * LIO Handle
                  , LabeledHandle, Handle
                  , IOMode(..)
                  , BufferMode(..)
                    -- * File operations
                  , openFile, openFileP
                  , hClose, hCloseP
                  , hFlush, hFlushP
                  , HandleOps(..)
                  , hGetP
                  , hGetNonBlockingP
                  , hGetContentsP
                  , hGetLineP
                  , hPutP
                  , hPutStrP
                  , hPutStrLnP
                  -- ** Special cases
                  , readFile, readFileP
                  , writeFile, writeFileP
                  -- * Directory operations
                  , getDirectoryContents, getDirectoryContentsP
                  , createDirectory, createDirectoryP
                  -- * Setting/getting handle status/settings
                  , hSetBuffering, hSetBufferingP
                  , hGetBuffering, hGetBufferingP
                  , hSetBinaryMode, hSetBinaryModeP
                  , hIsEOF, hIsEOFP
                  , hIsOpen, hIsOpenP
                  , hIsClosed, hIsClosedP
                  , hIsReadable, hIsReadableP
                  , hIsWritable, hIsWritableP
                  ) where

import Prelude hiding (readFile, writeFile)

import           Data.Serialize
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import           Control.Monad
import           Control.Exception (throwIO)


import           System.IO (IOMode(..), BufferMode(..), Handle)
import qualified System.IO as IO
import qualified System.Directory as IO
import           System.FilePath

import           LIO
import           LIO.Labeled.TCB
import           LIO.TCB
import           LIO.FS.TCB

--
-- LIO related
--

-- | Serialize 'MonadLIO'
type SMonadLIO l m = (SLabel l, MonadLIO l m)


-- | Same as 'evalLIO', but takes two additional parameters
-- corresponding to the path of the labeled filesystem store and the
-- label of the root. If the labeled filesystem store does not exist,
-- it is created at the specified path with the root having the
-- supplied label.
-- If the filesystem does exist, the supplied label is ignored and thus
-- unnecessary. However, if the root label is not provided and the
-- filesystem has not been initialized, a 'FSRootNeedLabel' exception
-- will be thrown.
evalWithRootFS :: SLabel l
               => FilePath   -- ^ Filesystem root
               -> Maybe l    -- ^ Label of root
               -> LIO l a    -- ^ LIO action
               -> LIOState l -- ^ Initial state
               -> IO a
evalWithRootFS path ml act = evalLIO (initFSTCB path ml >> act)

--
-- LIO Handle Operations
--

-- | Get the contents of a directory. The current label is raised to the
-- join of the current label and that of all the directories traversed to
-- the leaf directory. Note that, unlike the standard Haskell
-- 'getDirectoryContents', we first normalise the path by collapsing all
-- the @..@'s. The function uses 'unlabelFilePath' when raising the
-- current label and thus may throw an exception if the clearance is
-- too low.
-- /Note:/ The current LIO filesystem does not support links.
getDirectoryContents :: SMonadLIO l m => FilePath -> m [FilePath]
getDirectoryContents = getDirectoryContentsP NoPrivs

-- | Same as 'getDirectoryContents', but uses privileges when raising
-- the current label.
getDirectoryContentsP :: (SMonadLIO l m, Priv l p)
                      => p              -- ^ Privilege
                      -> FilePath       -- ^ Directory
                      -> m [FilePath]
getDirectoryContentsP p dir = do
  path <- taintObjPathP p dir
  liftLIO . rethrowIoTCB $ IO.getDirectoryContents path

-- | Create a directory at the supplied path with the given label.  The
-- given label must be bounded by the the current label and clearance, as
-- checked by 'guardAlloc'.  The current label (after traversing the
-- filesystem to the directory path) must flow to the supplied label,
-- which must, in turn, flow to the current label as required by
-- 'guardWrite'.
createDirectory :: SMonadLIO l m => l -> FilePath -> m ()
createDirectory = createDirectoryP NoPrivs

-- | Same as 'createDirectory', but uses privileges when raising the
-- current label and checking IFC restrictions.
createDirectoryP :: (SMonadLIO l m, Priv l p)
                 => p           -- ^ Privilege
                 -> l           -- ^ Label of new directory
                 -> FilePath    -- ^ Path of directory
                 -> m ()
createDirectoryP p l dir0 = do
  -- Check that the label is bounded by the current label and clearance:
  guardAllocP p l
  -- Clean up directory:
  dir  <- cleanUpPath dir0
  let (containingDir, dName) = breakDir dir
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  -- Get label of containing dir:
  ldir <- liftLIO $ getPathLabelTCB path
  -- Can write to containing dir:
  guardWriteP p ldir
  -- Can still create dir:
  guardAllocP p l
  -- Create actual directory:
  liftLIO $ createDirectoryTCB l $ path </> dName
    where breakDir dir = let ds  = splitDirectories dir
                             cd' = joinPath $ init ds
                             cd  = if null cd' then [pathSeparator] else cd'
                         in (cd, last ds)

--
-- Files
--

-- Synonym for a labeled handle.
type LabeledHandle l = Labeled l Handle

-- | Given a set of privileges, a (maybe) new label of a file, a
-- filepath, and the IO mode, open (and possibly create) the file. If the
-- file exists, the supplied label is not necessary; otherwise it must be
-- supplied.  The current label is raised to reflect all the traversed
-- directories.  Additionally the label of the file (new or existing)
-- must be between the current label and clearance, as imposed by
-- 'guardAlloc'. If the file is created, it is further required that the
-- current computation be able to write to the containing directory, as
-- imposed by 'guardWrite'.
openFile :: SMonadLIO l m
         => Maybe l    -- ^ Label of file if created
         -> FilePath   -- ^ File to open
         -> IOMode     -- ^ Mode
         -> m (LabeledHandle l)
openFile = openFileP NoPrivs

-- | Same as 'openFile', but uses privileges when traversing
-- directories and performing IFC checks.
openFileP :: (SMonadLIO l m, Priv l p)
          => p          -- ^ Privileges
          -> Maybe l    -- ^ Label of file if created
          -> FilePath   -- ^ File to open
          -> IOMode     -- ^ Mode
          -> m (LabeledHandle l)
openFileP p ml file' mode = do
  file <- cleanUpPath file'
  let containingDir = takeDirectory file
      fileName      = takeFileName  file
  -- Check that the supplied label is bounded by current label and clearance:
  maybe (return ()) (guardAllocP p) ml
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  --Get label of containing dir:
  ldir <- liftLIO $ getPathLabelTCB path
  -- Create actual file path:
  let objPath = path </> fileName
  -- Check if file exists:
  exists <- liftLIO . rethrowIoTCB $ IO.doesFileExist objPath
  if exists
     then do
       -- Get label of file:
       l <- liftLIO $ getPathLabelTCB objPath
       -- Make sure we can create labeled handle:
       guardAllocP p l
       -- NOTE: if mode == ReadMode, we might want to instead do
       -- guardAllocp p (l `lub` currentLabel) to allow opening     
       -- a handle for an object whose label is below the current
       -- label. Some Unix systems still update a file's atime
       -- when performing a read and so, for now, a read always
       -- implies a write.
       h <- liftLIO . rethrowIoTCB $ IO.openFile objPath mode
       return $ labelTCB l h
     else case ml of
           Nothing -> throwLIO FSObjNeedLabel
           Just l -> do
             -- Can write to containing dir:
             guardWriteP p ldir
             -- Can still create file with this label:
             guardAllocP p l
             h <- liftLIO $ createFileTCB l objPath mode
             return $ labelTCB l h

-- | Close a file handle. Must be able to write to the the labeled
-- handle, as checkd by 'guardWrite'.
hClose :: SMonadLIO l m => LabeledHandle l -> m ()
hClose = hCloseP NoPrivs

-- | Close a labeled file handle.
hCloseP :: (SMonadLIO l m, Priv l p) => p -> LabeledHandle l -> m ()
hCloseP p lh = do
  guardWriteP p (labelOf lh)
  liftLIO . rethrowIoTCB . IO.hClose $ unlabelTCB lh


-- | Flush a file handle. Must be able to write to the the labeled
-- handle, as checkd by 'guardWrite'.
hFlush :: SMonadLIO l m => LabeledHandle l -> m ()
hFlush = hFlushP NoPrivs

-- | Flush a labeled file handle.
hFlushP :: (SMonadLIO l m, Priv l p) => p -> LabeledHandle l -> m ()
hFlushP p lh = do
  guardWriteP p (labelOf lh)
  liftLIO . rethrowIoTCB . IO.hFlush $ unlabelTCB lh


-- | Class used to abstract reading and writing from and to handles,
-- respectively.
class Monad m => HandleOps h b m where
  hGet            :: h -> Int -> m b
  hGetNonBlocking :: h -> Int -> m b
  hGetContents    :: h -> m b
  hGetLine        :: h -> m b
  hPut            :: h -> b -> m ()
  hPutStr         :: h -> b -> m ()
  hPutStr         = hPut
  hPutStrLn       :: h -> b -> m ()

instance HandleOps IO.Handle L8.ByteString IO where
  hGet            = L8.hGet
  hGetNonBlocking = L8.hGetNonBlocking
  hGetContents    = L8.hGetContents
  hGetLine  h     = (L8.fromChunks . (:[])) `liftM` S8.hGetLine h
  hPut            = L8.hPut
  hPutStrLn       = L8.hPutStrLn

instance HandleOps IO.Handle S8.ByteString IO where
  hGet            = S8.hGet
  hGetNonBlocking = S8.hGetNonBlocking
  hGetContents    = S8.hGetContents
  hGetLine        = S8.hGetLine
  hPut            = S8.hPut
  hPutStrLn       = S8.hPutStrLn

instance (SLabel l, HandleOps IO.Handle b IO) =>
         HandleOps (LabeledHandle l) b (LIO l) where
  hGet            = hGetP NoPrivs
  hGetNonBlocking = hGetNonBlockingP NoPrivs
  hGetContents    = hGetContentsP NoPrivs
  hGetLine        = hGetLineP NoPrivs
  hPut            = hPutP NoPrivs
  hPutStrLn       = hPutStrLnP NoPrivs

-- | Read @n@ bytes from the labeled handle, using privileges when
-- performing label comparisons and tainting.
hGetP :: (Priv l p, Serialize l, HandleOps IO.Handle b IO)
      => p               -- ^ Privileges
      -> LabeledHandle l -- ^ Labeled handle
      -> Int             -- ^ Number of bytes to read
      -> LIO l b
hGetP p lh n = do
 guardWriteP p (labelOf lh)
 liftLIO . rethrowIoTCB $ hGet (unlabelTCB lh) n

-- | Same as 'hGetP', but will not block waiting for data to become
-- available. Instead, it returns whatever data is available.
-- Privileges are used in the label comparisons and when raising
-- the current label.
hGetNonBlockingP :: (Priv l p, Serialize l, HandleOps IO.Handle b IO)
                 => p -> LabeledHandle l -> Int -> LIO l b
hGetNonBlockingP p lh n = do
 guardWriteP p (labelOf lh)
 liftLIO . rethrowIoTCB $ hGetNonBlocking (unlabelTCB lh) n

-- | Read the entire labeled handle contents and close handle upon
-- reading @EOF@.  Privileges are used in the label comparisons
-- and when raising the current label.
hGetContentsP :: (Priv l p, Serialize l, HandleOps IO.Handle b IO)
              => p -> LabeledHandle l -> LIO l b
hGetContentsP p lh = do
 guardWriteP p (labelOf lh)
 liftLIO . rethrowIoTCB $ hGetContents (unlabelTCB lh)

-- | Read the a line from a labeled handle.
hGetLineP :: (Priv l p, Serialize l, HandleOps IO.Handle b IO)
          => p -> LabeledHandle l -> LIO l b
hGetLineP p lh = do
 guardWriteP p (labelOf lh)
 liftLIO . rethrowIoTCB $ hGetLine (unlabelTCB lh)

-- | Output the given (Byte)String to the specified labeled handle.
-- Privileges are used in the label comparisons and when raising
-- the current label.
hPutP :: (Priv l p, Serialize l, HandleOps IO.Handle b IO)
      => p -> LabeledHandle l -> b -> LIO l ()
hPutP p lh s = do
 guardWriteP p (labelOf lh)
 liftLIO . rethrowIoTCB $ hPut (unlabelTCB lh) s

-- | Synonym for 'hPutP'.
hPutStrP :: (Priv l p, Serialize l, HandleOps IO.Handle b IO)
          => p -> LabeledHandle l -> b -> LIO l ()
hPutStrP = hPutP

-- | Output the given (Byte)String with an appended newline to the
-- specified labeled handle. Privileges are used in the label
-- comparisons and when raising the current label.
hPutStrLnP :: (Priv l p, Serialize l, HandleOps IO.Handle b IO)
            => p -> LabeledHandle l -> b -> LIO l ()
hPutStrLnP p lh s = do
 guardWriteP p (labelOf lh)
 liftLIO . rethrowIoTCB $ hPutStrLn (unlabelTCB lh) s

--
-- Special cases
--

-- | Reads a file and returns the contents of the file as a ByteString.
readFile :: (HandleOps Handle b IO, SLabel l)
         => FilePath -> LIO l b
readFile = readFileP NoPrivs

-- | Same as 'readFile' but uses privilege in opening the file.
readFileP :: (HandleOps Handle b IO, Priv l p, Serialize l)
          => p -> FilePath -> LIO l b
readFileP p file = openFileP p Nothing file ReadMode >>= hGetContentsP p

-- | Write a ByteString to the given filepath with the supplied label.
writeFile :: (HandleOps Handle b IO, SLabel l)
          => l -> FilePath -> b -> LIO l ()
writeFile = writeFileP NoPrivs

-- | Same as 'writeFile' but uses privilege when opening, writing and
-- closing the file.
writeFileP  :: (HandleOps Handle b IO, Priv l p, Serialize l)
            => p -> l -> FilePath -> b -> LIO l ()
writeFileP p l file contents = do
  bracket (openFileP p (Just l) file WriteMode) (hCloseP p)
          (flip (hPutP p) contents)

--
-- Setting/getting handle status/setting
--

-- | Set the buffering mode
hSetBuffering :: SMonadLIO l m => LabeledHandle l -> BufferMode -> m ()
hSetBuffering = hSetBufferingP NoPrivs

-- | Set the buffering mode
hSetBufferingP :: (SMonadLIO l m, Priv l p)
               => p -> LabeledHandle l -> BufferMode -> m ()
hSetBufferingP p lh m = do
  guardWriteP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hSetBuffering (unlabelTCB lh) m

-- | Get the buffering mode
hGetBuffering :: SMonadLIO l m => LabeledHandle l -> m BufferMode
hGetBuffering = hGetBufferingP NoPrivs

-- | Get the buffering mode
hGetBufferingP :: (SMonadLIO l m, Priv l p)
               => p -> LabeledHandle l -> m BufferMode
hGetBufferingP p lh = do
  taintP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hGetBuffering (unlabelTCB lh)

-- | Select binary mode ('True') or text mode ('False')
hSetBinaryMode :: SMonadLIO l m => LabeledHandle l -> Bool -> m ()
hSetBinaryMode = hSetBinaryModeP NoPrivs

-- | Select binary mode ('True') or text mode ('False')
hSetBinaryModeP :: (SMonadLIO l m, Priv l p)
                => p -> LabeledHandle l -> Bool -> m ()
hSetBinaryModeP p lh m = do
  guardWriteP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hSetBinaryMode (unlabelTCB lh) m

-- | End of file.
hIsEOF :: SMonadLIO l m => LabeledHandle l -> m Bool
hIsEOF = hIsEOFP NoPrivs

-- | End of file.
hIsEOFP :: (SMonadLIO l m, Priv l p) => p -> LabeledHandle l -> m Bool
hIsEOFP p lh = do
  taintP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hIsEOF (unlabelTCB lh)
                                                                          
-- | Is handle open.                                                      
hIsOpen :: SMonadLIO l m => LabeledHandle l -> m Bool      
hIsOpen = hIsOpenP NoPrivs

-- | Is handle open.                                                      
hIsOpenP :: (SMonadLIO l m, Priv l p) => p -> LabeledHandle l -> m Bool      
hIsOpenP p lh = do
  taintP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hIsOpen (unlabelTCB lh)
                                                                          
-- | Is handle closed.                                                    
hIsClosed :: SMonadLIO l m => LabeledHandle l -> m Bool      
hIsClosed = hIsClosedP NoPrivs

-- | Is handle closed.                                                    
hIsClosedP :: (SMonadLIO l m, Priv l p) => p -> LabeledHandle l -> m Bool
hIsClosedP p lh = do
  taintP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hIsClosed (unlabelTCB lh)
                                                                          
-- | Is handle readable.                                                  
hIsReadable :: SMonadLIO l m => LabeledHandle l -> m Bool      
hIsReadable = hIsReadableP NoPrivs

-- | Is handle readable.                                                  
hIsReadableP :: (SMonadLIO l m, Priv l p) => p -> LabeledHandle l -> m Bool
hIsReadableP p lh = do
  taintP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hIsReadable (unlabelTCB lh)
                                                                          
-- | Is handle writable.                                                  
hIsWritable :: SMonadLIO l m => LabeledHandle l -> m Bool
hIsWritable = hIsWritableP NoPrivs

-- | Is handle writable.                                                  
hIsWritableP :: (SMonadLIO l m, Priv l p) => p -> LabeledHandle l -> m Bool
hIsWritableP p lh = do
  taintP p (labelOf lh)
  liftLIO . rethrowIoTCB $ IO.hIsWritable (unlabelTCB lh)

--
-- Internal helpers
--

-- | Given a pathname to a labeled filesystem object, traverse all the
-- directories up to the object, while correspondingly raising the
-- current label. Note that if the object or a parent-directory does not
-- exist, an exception will be thrown; the label of the exception will be
-- the join of all the directory labels up to the lookup failure.
--
-- /Note:/ this function cleans up the path before doing the
-- lookup, so e.g., path @/foo/bar/..@ will first be rewritten to @/foo@
-- and thus no traversal to @bar@.  Note that this is a more permissive
-- behavior than forcing the read of @..@ from @bar@.
-- @taintObjPath@ returns this cleaned up path.
taintObjPathP :: (SMonadLIO l m, Priv l p)
              => p         -- ^ Privilege 
              -> FilePath  -- ^ Path to object
              -> m FilePath
taintObjPathP p path0 = do
  -- Clean up supplied path:
  path <- cleanUpPath path0
  -- Get root directory:
  root <- liftLIO $ getRootDirTCB
  let dirs = splitDirectories . stripSlash $ path
  -- "Traverse" all directories up to object:
  forM_ ("" : allSubDirs dirs) $ \dir -> do
    l <- liftLIO $ getPathLabelTCB (root </> dir)
    taintP p l
  return $ root </> joinPath dirs

-- | Take a list of directories (e.g., @[\"a\",\"b\",\"c\"]@) and return
-- all the subtrees up to the node (@[\"a\",\"a/b\",\"a/b/c\"]@).
allSubDirs :: [FilePath] -> [FilePath]
allSubDirs dirs = reverse $ allSubDirs' dirs "" []
  where allSubDirs' []       _    acc = acc
        allSubDirs' (dir:[]) pfix acc = (pfix </> dir) : acc
        allSubDirs' (dir:ds) pfix acc = let ndir = pfix </> dir
                                        in allSubDirs' ds ndir (ndir : acc)

-- | Remove any 'pathSeparator's from the front of a file path.
stripSlash :: FilePath -> FilePath 
stripSlash [] = []
stripSlash xx@(x:xs) | x == pathSeparator = stripSlash xs
                     | otherwise          = xx

-- | Cleanup a file path, if it starts out with a @..@, we consider this
-- invalid as it can be used explore parts of the filesystem that should
-- otherwise be unaccessible. Similarly, we remove any @.@ from the path.
cleanUpPath :: MonadLIO l m => FilePath -> m FilePath 
cleanUpPath = liftLIO . rethrowIoTCB . doit . splitDirectories . normalise . stripSlash
  where doit []          = return []
        doit ("..":_)    = throwIO FSIllegalFileName
        doit (_:"..":xs) = doit xs
        doit (".":xs)    = doit xs
        doit (x:xs)      = (x </>) `liftM` doit xs
