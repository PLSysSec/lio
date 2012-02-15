{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module abstracts the basic 'FileHandle' methods provided by
-- the system library, and provides an 'LHandle' ('Labeled' 'Handle')
-- type that can be manipulated from within the 'LIO' Monad.
-- (There is no notion of changeable current working directory in
-- the 'LIO' Monad, nor symbolic links.)
--
-- The actual storage of labeled files is handled by the "LIO.FS"
-- module.
--
-- /IMPORTANT:/ To use a labeled filesystem you must use 'evalWithRoot',
-- otherwise any actions built using the combinators of this module will
-- crash.
--
-- An example use is shown below: 
--
-- >
-- >  main = dcEvalWithRoot "/tmp/lioFS" $ do
-- >    createDirectoryP p lsecrets "secrets"
-- >    writeFileP p ("secrets" </> "alice" ) "I like Bob!"
-- >      where p = ...
-- >            lsecrets = ....
-- >
--
-- The file store for the labeled filesystem (see "LIO.FS") will
-- be created in @\/tmp\/lioFS@, but this is transparent and the user
-- can think of the filesystem as having root @/@.
module LIO.Handle (
                  -- * LIO with filesystem support
                    evalWithRoot
                  -- * Generic Handle operations
                  , DirectoryOps(..)
                  , CloseOps(..)
                  , HandleOps(..)
                  , readFile, writeFile, writeFileL
                  , IOMode(..)
                  -- * LIO Handle
                  , LHandle, labelOfHandle 
                  -- ** Privileged combinators
                  , getDirectoryContentsP
                  , createDirectoryP
                  , openFileP
                  , hCloseP
                  , hFlushP 
                  , hGetP
                  , hGetNonBlockingP
                  , hGetContentsP
                  , hPutP
                  , hPutStrP
                  , hPutStrLnP
                  , readFileP, writeFileP, writeFileLP
                  ) where


#if __GLASGOW_HASKELL__ >= 702
import safe Prelude hiding (catch, readFile, writeFile)
import safe System.IO (IOMode(..))
import safe qualified System.IO as IO
#else
import Prelude hiding (catch, readFile, writeFile)
import System.IO (IOMode(..))
import qualified System.IO as IO
#endif

import LIO.TCB
import LIO.FS
import Data.Serialize
import qualified System.Directory as IO
import System.FilePath
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC


-- | Class used to abstract reading and creating directories, and
-- opening (possibly creating) files.
class (Monad m) => DirectoryOps h m | m -> h where
  -- | Get the contents of a directory.
  getDirectoryContents :: FilePath -> m [FilePath]
  -- | Create a directory at the supplied path.
  -- The LIO instance labels the directory with the current label.
  createDirectory      :: FilePath -> m ()
  -- | Open handle to manage the file at the supplied path.
  openFile             :: FilePath -> IOMode -> m h

-- | Class used to abstract close and flush operations on handles.
class (Monad m) => CloseOps h m where
  hClose :: h -> m ()
  hFlush :: h -> m ()

-- | Class used to abstract reading and writing from and to handles,
-- respectively.
class (CloseOps h m) => HandleOps h b m where
  hGet            :: h -> Int -> m b
  hGetNonBlocking :: h -> Int -> m b
  hGetContents    :: h -> m b
  hPut            :: h -> b -> m ()
  hPutStr         :: h -> b -> m ()
  hPutStr         = hPut
  hPutStrLn       :: h -> b -> m ()

--
-- Standard IO Handle Operations
--

instance DirectoryOps IO.Handle IO where
  getDirectoryContents = IO.getDirectoryContents
  createDirectory      = IO.createDirectory
  openFile             = IO.openBinaryFile

instance CloseOps IO.Handle IO where
  hClose = IO.hClose
  hFlush = IO.hFlush

instance HandleOps IO.Handle L.ByteString IO where
  hGet            = L.hGet
  hGetNonBlocking = L.hGetNonBlocking
  hGetContents    = L.hGetContents
  hPut            = L.hPut
  hPutStrLn       = LC.hPutStrLn

--
-- LIO Handle Operations
--

-- | A labeled handle.
data LHandle l = LHandleTCB l IO.Handle

-- | Get the label of a labeled handle.
labelOfHandle :: Label l => LHandle l -> l
labelOfHandle (LHandleTCB l _) = l

instance (Serialize l, LabelState l p s)
          => DirectoryOps (LHandle l) (LIO l p s) where
  getDirectoryContents = getDirectoryContentsP noPrivs
  createDirectory  f   = do l <- getLabel 
                            createDirectoryP noPrivs l f
  openFile f m         = do l <- getLabel 
                            openFileP noPrivs (Just l) f m

-- | Get the contents of a directory. The current label is raised to
-- the join of the current label and that of all the directories
-- traversed to the leaf directory (of course, using privileges to
-- keep the current label unchanged when possible). Note that, unlike
-- the standard Haskell 'getDirectoryContents', we first normalise the
-- path by collapsing all the @..@'s. (The LIO filesystem does not
-- support links.)
getDirectoryContentsP :: (LabelState l p s, Serialize l)
                      => p              -- ^ Privilege
                      -> FilePath       -- ^ Directory
                      -> LIO l p s [FilePath]
getDirectoryContentsP p' dir = withCombinedPrivs p' $ \p -> do
  path <- lookupObjPathP p dir >>= unlabelFilePathP p
  rtioTCB $ IO.getDirectoryContents path

-- | Create a directory at the supplied path with the given label.
-- The current label (after traversing the filesystem to the
-- directory path) must flow to the supplied label which in turn must
-- flow to the current label (of course, using privileges to bypass
-- certain restrictions). If this information flow restriction is
-- satisfied, the directory is created.
createDirectoryP :: (LabelState l p s, Serialize l)
                 => p           -- ^ Privilege
                 -> l           -- ^ Label of new directory
                 -> FilePath    -- ^ Path of directory
                 -> LIO l p s ()
createDirectoryP p ldir path' = withCombinedPrivs p $ \priv -> do
  path <- cleanUpPath path'
  aguardP priv ldir
  lcDir <- lookupObjPathP priv (containingDir path)
  wguardP priv $ labelOfFilePath lcDir
  rtioTCB $ createDirectoryTCB ldir path
    where stripLastSlash = (reverse . stripSlash . reverse)
          containingDir = takeDirectory . ([pathSeparator] </>)
                                        .  stripLastSlash


-- | Given a set of privileges, a new (maybe) label of a file, a filepath
-- and the handle mode, open (and possibly create) the file. If the file 
-- exists the supplied label is not necessary; otherwise it must be supplied.
-- The current label is raised to reflect all the traversed directories 
-- (of course, using privileges to minimize the taint). Additionally the
-- label of the file (new or existing) must be between the current label
-- and clearance. If the file is created, it is further required that the 
-- current process be able to write to the containing directory.
openFileP :: (LabelState l p s, Serialize l)
          => p          -- ^ Privileges
          -> Maybe l    -- ^ Label of file if created
          -> FilePath   -- ^ File to open
          -> IOMode     -- ^ Mode of handle
          -> LIO l p s (LHandle l)
openFileP p mlfile path' mode = withCombinedPrivs p $ \priv -> do
  path <- cleanUpPath path'
  let containingDir = takeDirectory path
      fileName      = takeFileName  path
  -- check that the supplied label is bounded by current label and clearance:
  maybe (return ()) (aguardP priv) mlfile
  -- lookup object corresponding to containing dir:
  lcDir <- lookupObjPathP priv containingDir 
  -- unlabel the containing dir object:
  actualCDir <- unlabelFilePathP priv lcDir  
  let objPath = actualCDir </> fileName -- actual object path
  exists <- rtioTCB $ IO.doesFileExist objPath
  if exists
     then do l <- getObjLabelTCB objPath -- label of object
             aguardP priv l -- make sure we can actually read the file
             -- NOTE: if mode == ReadMode, we might want to instead do
             -- aguardP priv (l `lub` currentLabel) to allow opening     
             -- a handle for an object whose label is below the current
             -- label. Some Unix systems still update a file's atime
             -- when performing a read and so, for now, a read always
             -- implies a write.
             h <- rtioTCB $ IO.openFile objPath mode
             return $ LHandleTCB l h
     else case mlfile of
           Nothing -> throwIO $ userError "openFileP: File label missing."
           Just l -> do
             wguardP priv (labelOfFilePath lcDir) -- can write to containing dir
             aguardP priv l -- make sure we can actually read the file
             -- NOTE: the latter is necessary as looking up the containing
             -- directory object might have raised the current label.
             h <- ioTCB $ createFileTCB l objPath mode
             return $ LHandleTCB l h
            

instance (LabelState l p s) => CloseOps (LHandle l) (LIO l p s) where
  hClose = hCloseP noPrivs
  hFlush = hFlushP noPrivs

instance (LabelState l p s, CloseOps (LHandle l) (LIO l p s)
         , HandleOps IO.Handle b IO) =>
           HandleOps (LHandle l) b (LIO l p s) where
  hGet            = hGetP noPrivs
  hGetNonBlocking = hGetNonBlockingP noPrivs
  hGetContents    = hGetContentsP noPrivs
  hPut            = hPutP noPrivs
  hPutStrLn       = hPutStrLnP noPrivs

-- | Close a labeled file handle.
hCloseP :: (LabelState l p s) => p -> LHandle l -> LIO l p s ()
hCloseP p' (LHandleTCB l h) = withCombinedPrivs p' $ \p ->
  wguardP p l >> rtioTCB (hClose h)

-- | Flush a labeled file handle.
hFlushP :: (LabelState l p s) => p -> LHandle l -> LIO l p s ()
hFlushP p' (LHandleTCB l h) = withCombinedPrivs p' $ \p ->
  wguardP p l >> rtioTCB (hFlush h)

-- | Read @n@ bytes from the labeled handle, using privileges when
-- performing label comparisons and tainting.
hGetP :: (LabelState l p s, HandleOps IO.Handle b IO)
      => p              -- ^ Privileges
      -> LHandle l      -- ^ Labeled handle
      -> Int            -- ^ Number of bytes to read
      -> LIO l p s b
hGetP p' (LHandleTCB l h) n = withCombinedPrivs p' $ \p ->
  wguardP p l >> rtioTCB (hGet h n)

-- | Same as 'hGetP', but will not block waiting for data to become
-- available. Instead, it returns whatever data is available.
-- Privileges are used in the label comparisons and when raising
-- the current label.
hGetNonBlockingP :: (LabelState l p s, HandleOps IO.Handle b IO)
                 => p -> LHandle l -> Int -> LIO l p s b
hGetNonBlockingP p' (LHandleTCB l h) n = withCombinedPrivs p' $ \p ->
  wguardP p l >> rtioTCB (hGetNonBlocking h n)

-- | Read the entire labeled handle contents and close handle upon
-- reading @EOF@.  Privileges are used in the label comparisons
-- and when raising the current label.
hGetContentsP :: (LabelState l p s, HandleOps IO.Handle b IO)
              => p -> LHandle l -> LIO l p s b
hGetContentsP p' (LHandleTCB l h) = withCombinedPrivs p' $ \p ->
  wguardP p l >> rtioTCB (hGetContents h)

-- | Output the given (Byte)String to the specified labeled handle.
-- Privileges are used in the label comparisons and when raising
-- the current label.
hPutP :: (LabelState l p s, HandleOps IO.Handle b IO)
      => p -> LHandle l -> b -> LIO l p s ()
hPutP p' (LHandleTCB l h) s = withCombinedPrivs p' $ \p ->
  wguardP p l >> rtioTCB (hPut h s)

-- | Synonym for 'hPutP'.
hPutStrP :: (LabelState l p s, HandleOps IO.Handle b IO)
          => p -> LHandle l -> b -> LIO l p s ()
hPutStrP = hPutP

-- | Output the given (Byte)String with an appended newline to the
-- specified labeled handle. Privileges are used in the label
-- comparisons and when raising the current label.
hPutStrLnP :: (LabelState l p s, HandleOps IO.Handle b IO)
            => p -> LHandle l -> b -> LIO l p s ()
hPutStrLnP p' (LHandleTCB l h) s = withCombinedPrivs p' $ \p ->
  wguardP p l >> rtioTCB (hPutStrLn h s)

--
-- Special cases
--

-- | Reads a file and returns the contents of the file as a (Byte)String.
readFile :: (DirectoryOps h m, HandleOps h b m) => FilePath -> m b
readFile path = openFile path ReadMode >>= hGetContents

-- | Write a (Byte)String to a file.
writeFile :: (DirectoryOps h m, HandleOps h b m, OnExceptionTCB m)
          => FilePath -> b -> m ()
writeFile path contents = bracketTCB (openFile path WriteMode) hClose
                          (flip hPut contents)

-- | Same as 'readFile' but uses privilege in opening the file.
readFileP :: (HandleOps IO.Handle b IO, LabelState l p s, Serialize l) =>
             p -> FilePath -> LIO l p s b
readFileP p' path =  withCombinedPrivs p' $ \p ->
  openFileP p Nothing path ReadMode >>= hGetContentsP p

-- | Same as 'writeFile' but uses privilege in opening the file.
writeFileP  :: (HandleOps IO.Handle b IO, LabelState l p s, Serialize l) =>
               p -> FilePath -> b -> LIO l p s ()
writeFileP p' path contents = withCombinedPrivs p' $ \privs -> do
  l <- getLabel
  bracketTCB (openFileP privs (Just l) path WriteMode) (hCloseP privs)
             (flip (hPutP privs) contents)

-- | Same as 'writeFile' but also takes the label of the file.
writeFileL  :: (HandleOps IO.Handle b IO, LabelState l p s, Serialize l) =>
               l -> FilePath -> b -> LIO l p s ()
writeFileL = writeFileLP noPrivs

-- | Same as 'writeFileL' but uses privilege in opening the file.
writeFileLP  :: (HandleOps IO.Handle b IO, LabelState l p s, Serialize l) =>
               p -> l -> FilePath -> b -> LIO l p s ()
writeFileLP p' l path contents = withCombinedPrivs p' $ \privs -> do
  bracketTCB (openFileP privs (Just l) path WriteMode) (hCloseP privs)
             (flip (hPutP privs) contents)
