{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances,
             MultiParamTypeClasses #-}
{- | 

This module provides a very simple API for interacting with a labeled
filesystem.  Each file and directory hsa an associated label that is
used to track and control the information flowing to/from the
file/directory. The API exposed by this module is analogous to a
subset of the "System.IO" API. We currently do not allow operations on
file handles. Rather, files must be read read and written to in whole
(as strict ByteStrings).

The actual storage of labeled files is handled by the "LIO.FS.TCB"
module.  The filesystem is implemented as a file store in which labels
are associated with files and directories using, extended attributes.

/IMPORTANT:/ To use the labeled filesystem you must use 'withLIOFS'
(or other initializers), otherwise any actions built using the
combinators of this module will crash.

An example use case shown below: 

>  import LIO.FS.Simple
>  import LIO.FS.Simple.DCLabel
>
>  main = withDCFS "/tmp/lioFS" $ evalDC $ do
>    createDirectoryP p lsecrets "secrets"
>    writeFileP p ("secrets" </> "alice" ) "I like Bob!"
>      where p = ...
>            lsecrets = ....
>

The file store for the labeled filesystem (see "LIO.FS.TCB") will
be created in @\/tmp\/lioFS@, but this is transparent and the user
can think of the filesystem as having root @/@. Note that for this to
work the filesystem must be mounted with the @user_xattr@ option.
For example, on GNU/Linux, you can remount your drive:

> mount -o remount -o user_xattr devicename

In the current version of the filesystem, there is no notion of
changeable current working directory in the 'LIO' Monad, nor symbolic
links.

-}
module LIO.FS.Simple (
  -- * Initializing labeled filesystem
    initializeLIOFS, withLIOFS 
  -- * File operations
  , readFile, readFileP
  , writeFile, writeFileP
  , appendFile, appendFileP
  , removeFile, removeFileP
  , labelOfFile, labelOfFileP
  -- * Directory operations
  , getDirectoryContents, getDirectoryContentsP
  , createDirectory, createDirectoryP
  , removeDirectory, removeDirectoryP
  -- * Labeled file/directory operations
  , lReadFile, lReadFileP
  , lGetDirectoryContents, lGetDirectoryContentsP
  -- * Filesystem errors
  , FSError(..)
  -- * Misc helpers
  , cleanUpPath, taintObjPathP, labelDirectoryRecursively 
  ) where

import Prelude hiding (readFile, writeFile, appendFile)

import safe qualified Data.ByteString.Char8 as S8

import safe Control.Monad
import safe Control.Exception (throwIO)


import safe System.IO (IOMode(..))
import safe qualified System.IO as IO
import safe qualified System.IO.Error as IO
import safe qualified System.Directory as IO
import safe System.FilePath
import safe System.Posix.Files

import safe LIO
import safe LIO.Error
import LIO.TCB
import LIO.FS.TCB


--
-- LIO directory operations
--

-- | Get the contents of a directory. The current label is raised to the
-- join of the current label and that of all the directories traversed to
-- the leaf directory. Note that, unlike the standard Haskell
-- 'getDirectoryContents', we first normalise the path by collapsing all
-- the @..@'s. The function uses 'unlabelFilePath' when raising the
-- current label and thus may throw an exception if the clearance is
-- too low.
-- /Note:/ The current LIO filesystem does not support links.
getDirectoryContents :: MonadLIO l m => FilePath -> m [FilePath]
getDirectoryContents = getDirectoryContentsP noPrivs

-- | Same as 'getDirectoryContents', but uses privileges when raising
-- the current label.
getDirectoryContentsP :: (MonadLIO l m, PrivDesc l p)
                      => Priv p         -- ^ Privilege
                      -> FilePath       -- ^ Directory
                      -> m [FilePath]
getDirectoryContentsP p dir = liftLIO $ withContext "getDirectoryContentsP" $ do
  path <- taintObjPathP p dir
  ioTCB $ IO.getDirectoryContents path

-- | Same as 'getDirectoryContentsP', but returns a labeled list of
-- the directory contents, thus only taining the context up to the
-- containing directory.
lGetDirectoryContentsP :: (MonadLIO l m, PrivDesc l p)
                       => Priv p         -- ^ Privilege
                       -> FilePath       -- ^ Directory
                       -> m (Labeled l [FilePath])
lGetDirectoryContentsP p dir = liftLIO $
  withContext "lGetDirectoryContentsP" $ do
    path <- cleanUpPath dir
    -- Taint up to containing dir, get label of directory
    l <- labelOfFileP p path
    -- Check that the label is bounded by the current label and clearance:
    guardAllocP p l
    -- Read the directory content
    ls <- ioTCB $ IO.getDirectoryContents path
    labelP p l ls

-- | Same as 'getDirectoryContents', but returns a labeled list of
-- the directory contents, thus only taining the context up to the
-- containing directory.
lGetDirectoryContents :: MonadLIO l m => FilePath -> m (Labeled l [FilePath])
lGetDirectoryContents = lGetDirectoryContentsP noPrivs

-- | Create a directory at the supplied path with the given label.  The
-- given label must be bounded by the the current label and clearance, as
-- checked by 'guardAlloc'.  The current label (after traversing the
-- filesystem to the directory path) must flow to the supplied label,
-- which must, in turn, flow to the current label as required by
-- 'guardWrite'.
createDirectory :: MonadLIO l m => l -> FilePath -> m ()
createDirectory = createDirectoryP noPrivs

-- | Same as 'createDirectory', but uses privileges when raising the
-- current label and checking IFC restrictions.
createDirectoryP :: (MonadLIO l m, PrivDesc l p)
                 => Priv p      -- ^ Privilege
                 -> l           -- ^ Label of new directory
                 -> FilePath    -- ^ Path of directory
                 -> m ()
createDirectoryP p l file' = liftLIO $ withContext "createDirectoryP" $ do
  file <- cleanUpPath file'
  let containingDir = takeDirectory file
      fileName      = takeFileName  file
  -- Check that the label is bounded by the current label and clearance:
  guardAllocP p l
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  -- Get label of containing dir:
  ldir <- ioTCB $ getPathLabelTCB path
  -- Can write to containing dir:
  guardWriteP p ldir
  -- Can still create dir:
  guardAllocP p l
  -- Create actual directory:
  createDirectoryTCB l $ path </> fileName


-- | Same as 'lReadFile' but uses privilege traversing directories.
lReadFileP :: (MonadLIO l m, PrivDesc l p)
           => Priv p     -- ^ Privileges
           -> FilePath   -- ^ File to open
           -> m (Labeled l S8.ByteString)
lReadFileP p file' = liftLIO $ withContext "lReadFileP" $ do
  file <- cleanUpPath file'
  let containingDir = takeDirectory file
      fileName      = takeFileName  file
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  -- Create actual file path:
  let objPath = path </> fileName
  -- Check if file exists:
  exists <- ioTCB $ IO.doesFileExist objPath
  if exists
    then do
      -- Get label of file:
      l <- ioTCB $ getPathLabelTCB objPath
      -- Make sure that the file label is not above the clearance
      guardAllocP p l
      -- Read file:
      bs <- ioTCB $ S8.readFile objPath
      labelP p l bs
    else do void . ioTCB $ S8.readFile objPath
            throwLIO (userError "BUG: file should not exist")

-- | Reads a file and returns the contents of the file as a labeled
-- strict ByteString.  The current label is raised to reflect all the
-- traversed directories. This differs from 'readFile' in not
-- automatically raising the current label to the label of the file.
lReadFile :: MonadLIO l m => FilePath -> m (Labeled l S8.ByteString)
lReadFile = lReadFileP noPrivs

-- | Same as 'readFile' but uses privilege in opening the file.
readFileP :: (MonadLIO l m, PrivDesc l p)
           => Priv p     -- ^ Privileges
           -> FilePath   -- ^ File to open
           -> m S8.ByteString
readFileP priv name = do
  lf <- lReadFileP priv name
  liftLIO $ unlabelP priv lf

-- | Reads a file and returns the contents of the file as a strict
-- ByteString.  The current label is raised to reflect all the
-- traversed directories.  If the file exists it is further raised to
-- the label of the file to reflect the read.
readFile :: MonadLIO l m => FilePath -> m S8.ByteString
readFile = readFileP noPrivs

-- | Same as 'writeFile' but uses privilege when writing to the file.
writeFileP  :: (PrivDesc l p, MonadLIO l m)
            => Priv p -> Maybe l -> FilePath -> S8.ByteString -> m ()
writeFileP p ml file' contents = liftLIO $ withContext "writeFileP" $ do
  file <- cleanUpPath file'
  let containingDir = takeDirectory file
      fileName      = takeFileName  file
  -- Check that the supplied label is bounded by current label and clearance:
  maybe (return ()) (guardAllocP p) ml
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  -- Create actual file path:
  let objPath = path </> fileName
  -- Check if file exists:
  exists <- ioTCB $ IO.doesFileExist objPath
  if exists
     then do
       -- Get label of file:
       l <- ioTCB $ getPathLabelTCB objPath
       -- Make sure that the provided label (if any) can flow to this
       -- label: the user of this function may assume that the
       -- supplied label is used to protect the contents, so we should
       -- ensure that they get /at least/ that degree of protection
       case ml of
         Just lopt | not (canFlowTo lopt l) ->
           labelError  "Supplied label does not flow to label of file" [lopt, l]
         _ -> return ()
       -- Make sure we can write to the file:
       guardWriteP p l
       ioTCB $ S8.writeFile objPath contents
     else case ml of
           Nothing -> throwLIO FSObjNeedLabel
           Just l -> do
             -- Get label of containing dir:
             ldir <- ioTCB $ getPathLabelTCB path
             -- Make sure we can write to containing dir:
             guardWriteP p ldir
             -- Make sure that we can still create file
             guardAllocP p l
             -- Write to the file
             bracket (createBinaryFileTCB l objPath WriteMode)
                     (ioTCB . IO.hClose) 
                     (\h -> ioTCB $ S8.hPut h contents)


-- | Given an optional label, file path and string, write the string
-- to the file at specified path. The optional label (which must be
-- bounded by the current label and clearance, as enforced by
-- 'guardAlloc') is used to set the label on the file, if the file
-- does not already exist; otherwise the label must flow to the label
-- of the file. (Supplying a 'Nothing' is the same as 'Just' supplying
-- the current label.) This function ensures that current label is
-- raised to reflect all the traversed directories.  Note that if the
-- file does not already exist, it is further required that the
-- current computation be able to write to the containing directory,
-- as imposed by 'guardWrite'.
writeFile :: MonadLIO l m => Maybe l -> FilePath -> S8.ByteString -> m ()
writeFile = writeFileP noPrivs

-- | Same as 'appendFile' but uses privilege when writing to the file.
appendFileP  :: (PrivDesc l p, MonadLIO l m)
             => Priv p -> FilePath -> S8.ByteString -> m ()
appendFileP p file' contents = liftLIO $ withContext "appendFileP" $ do
  file <- cleanUpPath file'
  let containingDir = takeDirectory file
      fileName      = takeFileName  file
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  -- Create actual file path:
  let objPath = path </> fileName
  -- Check if file exists:
  exists <- ioTCB $ IO.doesFileExist objPath
  if exists
     then do
       -- Get label of file:
       l <- ioTCB $ getPathLabelTCB objPath
       -- Make sure we can write-only to the file:
       guardAllocP p l
       ioTCB $ S8.appendFile objPath contents
    else throwLIO $ IO.mkIOError IO.doesNotExistErrorType
                                 "appendFileP" Nothing (Just objPath)

-- | Given a file path and string, append the string to the file at
-- specified path. This function ensures that current label is raised
-- to reflect all the traversed directories.  Moreover, it requires
-- that the file this is appending to exists and its label is bounded
-- by the current label and clearance (as enforced by 'guardAlloc').
appendFile :: MonadLIO l m => FilePath -> S8.ByteString -> m ()
appendFile = appendFileP noPrivs

-- | Get the label of a file/director at the supplied file path.  The
-- current label is raised to reflect all the traversed directories.
labelOfFile :: MonadLIO l m => FilePath -> m l
labelOfFile = labelOfFileP noPrivs

-- | Same as 'labelOfFile' but uses privilege in traversing
-- directories.
labelOfFileP :: (MonadLIO l m, PrivDesc l p)
          => Priv p     -- ^ Privileges
          -> FilePath   -- ^ File to get the label of
          -> m l
labelOfFileP p file' = liftLIO $ withContext "labelOfFileP" $ do
  file <- cleanUpPath file'
  let containingDir = takeDirectory file
      fileName      = takeFileName  file
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  -- Create actual file path:
  let objPath = path </> fileName
  -- Check if file exists:
  exists <- ioTCB $ IO.doesFileExist objPath
  -- Get the label of the file
  if exists 
    then ioTCB $ getPathLabelTCB objPath
    else throwLIO $ IO.mkIOError 
                      IO.doesNotExistErrorType
                     "labelOfFileP" Nothing (Just objPath)

-- | Remove the file at the specified path. The current computation
-- must be able to both write to the file and containing directory.
-- Moreover, the current label is raised to reflect the traversal of
-- directories up to the file.
removeFile :: MonadLIO l m => FilePath -> m ()
removeFile f = liftLIO $ withContext "removeFile" $ 
                 removeFileOrDirP "removeFile" False noPrivs f

-- | Same as 'removeFile', but uses privileges to carry out the
-- actions.
removeFileP :: (MonadLIO l m, PrivDesc l p) => Priv p -> FilePath -> m ()
removeFileP p f = liftLIO $ withContext "removeFileP" $ 
                    removeFileOrDirP "removeFileP" False p f

-- | Same as 'removeFile', but removes a directory.
removeDirectory :: MonadLIO l m => FilePath -> m ()
removeDirectory f = liftLIO $ withContext "removeDirectory" $ 
                      removeFileOrDirP "removeDirectory" True noPrivs f

-- | Same as 'removeDirectory', but uses privileges to carry out the
-- actions.
removeDirectoryP :: (MonadLIO l m, PrivDesc l p) 
            => Priv p -> FilePath -> m ()
removeDirectoryP p f = liftLIO $ withContext "removeDirectoryP" $ 
                         removeFileOrDirP "removeDirectoryP" True p f


-- | Remove a file or directory. See 'removeFile' for a high level
-- description of the underlying actions.
removeFileOrDirP :: PrivDesc l p
                 => String -> Bool -> Priv p -> FilePath -> LIO l ()
removeFileOrDirP ctx isDir p file' = do
  file <- cleanUpPath file'
  let containingDir = takeDirectory file
      fileName      = takeFileName  file
  -- Taint up to containing dir:
  path <- taintObjPathP p containingDir
  -- Get label of containing dir:
  ldir <- ioTCB $ getPathLabelTCB path
  -- Can write to containing dir:
  guardWriteP p ldir
  -- Create actual file path:
  let objPath = path </> fileName
  -- Check if file exists:
  ok <- ioTCB $ exists objPath
  if ok
    then do -- Get label of file:
            l <- ioTCB $ getPathLabelTCB objPath
            -- Make sure we can write to the file:
            guardWriteP p l
            ioTCB $ remove objPath
    else throwLIO $ IO.mkIOError IO.doesNotExistErrorType
                                 ctx Nothing (Just objPath)
    where (exists, remove) = if isDir
                               then (IO.doesDirectoryExist, IO.removeDirectory)
                               else (IO.doesFileExist, IO.removeFile)

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
taintObjPathP :: (MonadLIO l m, PrivDesc l p)
              => Priv p         -- ^ Privilege 
              -> FilePath  -- ^ Path to object
              -> m FilePath
taintObjPathP p path0 = liftLIO $ do
  -- Clean up supplied path:
  path <- cleanUpPath path0
  -- Get root directory:
  root <- getRootDirTCB
  let dirs = splitDirectories . stripSlash $ path
  -- "Traverse" all directories up to object:
  forM_ ("" : allSubDirs dirs) $ \dir -> do
    l <- ioTCB $ getPathLabelTCB (root </> dir)
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

-- | Class for generating clean filepaths
class CleanUpPath m where
  -- | Cleanup a file path, if it starts out with a @..@, we consider this
  -- invalid as it can be used explore parts of the filesystem that should
  -- otherwise be unaccessible. Similarly, we remove any @.@ from the path.
  cleanUpPath :: FilePath -> m FilePath 

instance CleanUpPath IO where
  cleanUpPath = doit . splitDirectories . normalise . stripSlash
    where doit []          = return []
          doit ("..":_)    = throwIO FSIllegalFileName
          doit (_:"..":xs) = doit xs
          doit (".":xs)    = doit xs
          doit (x:xs)      = (x </>) `liftM` doit xs

instance Label l => CleanUpPath (LIO l) where
  cleanUpPath = ioTCB . cleanUpPath


-- | Label the directory and every file within recursively with the
-- supplied label. Note this funciton expects a full path.
labelDirectoryRecursively :: Label l => l -> FilePath -> IO ()
labelDirectoryRecursively l dir = do
  exists <- IO.doesDirectoryExist dir
  unless exists $ throwIO $ IO.mkIOError IO.doesNotExistErrorType
                                        ctx Nothing (Just dir)
  setPathLabelTCB dir l
  fs <- filter (\f -> f `notElem` [".", ".."]) `liftM` IO.getDirectoryContents dir
  forM_ fs $ \f -> do
    let file = dir </> f
    stat <- getFileStatus file
    case () of
      _ | isRegularFile stat -> setPathLabelTCB file l
      _ | isDirectory stat   -> labelDirectoryRecursively l file
      _ -> throwIO $ IO.mkIOError IO.illegalOperationErrorType ctx
                                  Nothing (Just file)

  where ctx = "labelDirectoryRecursively"
