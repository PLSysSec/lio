{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE SafeImports #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module implements a labeled filesystem as a file store in
   which a label is associated with every file and directory. The
   module "LIO.Handle" provides an interface for working with labeled
   directories and files -- this module provides the underlying
   low-level (mostly trusted) interface.

   The file store contains 3 components:

   * @.magic@ file containing a sequence of pre-defined bytes. The
     existence of this file and correct contents indicates that
     the file store and root of the labeled filesystem were created
     before any system crash.

   * @.obj@ directory containing all the objects (files and
     directories) in the filesystem, organized according to the
     object's label.

   * @ROOT@ symbolic link pointing to a directory object in @.obj@,
     representing the root of the filesystem.

   As the details of @.magic@ and @ROOT@ are straight forward, we
   now focus on the details of @.obj@.

   Each object is stored in @.obj@ in a directory corresponding
   to the label of the file. For example, an object protected by
   @bob@'s label will exist as:

   > .obj/oTAUzOt5YoQAyThw89eSgWuig-0=/obj012345

   where @oTAUzOt5YoQAyThw89eSgWuig-0=@ is the base64url encoding of
   the (SHA-224 hash of) label @bob@, and @obj012345@ is the object's
   name. Each label directory also contains a @.label@ file whose
   contents correspond to the serialization of the label. Hence, for
   example,

   > .obj/oTAUzOt5YoQAyThw89eSgWuig-0=/.label

   would have @bob@'s label.
   
   Every object in a label directory is either a file or a directory.
   If the object is a directory, its contents will strictly have
   symbolic links to objects in the file store. This is more easily
   understood using an example.

   Consider the following familiar filesystem layout, containing 4
   directories and 2 files:

   >
   > /
   > |-- bobOralice      (type: directory, label: bob \/ alice)
   > |   |-- alice       (type: directory, label: alice)
   > |   |-- bob         (type: directory, label: bob)
   > |   |   '-- secret  (type: file     , label: bob)
   > |   '-- messages    (type: file     , label: bob \/ alice)
   > '-- clarice         (type: directory, label: clarice)
   >

   Suppose we take @/lioFS@ as the location of the file store for our
   labeled filesystem. The above files and directories will have the
   same layout in our file system, but each file/directory points to
   an object in the object store (@.obj@), as shown below. 

   >
   > /lioFS/ROOT
   > |-- bobOralice -> ../../c_EkXYGrmrSit9j_8VjP_-8DgaM=/objXIBabq
   > |   |-- alice -> ../../12to8q3vDp7ApyKEQk2LQ_2nrvs=/objqZeR5w
   > |   |-- bob -> ../../oTAUzOt5YoQAyThw89eSgWuig-0=/objtsePnc
   > |   |   '-- secret -> ../../oTAUzOt5YoQAyThw89eSgWuig-0=/objFIQCIm
   > |   '-- messages -> ../../c_EkXYGrmrSit9j_8VjP_-8DgaM=/objQaiwuA
   > '-- clarice -> ../../wordEFmBzWc7Q6qP-TetDgZaG8A=/objOMh0mj
   >

   Note that, to a user, the interface (see "LIO.Handle") is
   unchanged and they do not need to handle or understand the
   underlying file store. We refer to this view as \"friendly\". The
   file store for the above layout is:

   >
   > /lioFS/
   > |-- .magic
   > |-- .obj
   > |   |-- 12to8q3vDp7ApyKEQk2LQ_2nrvs=
   > |   |   |-- .label (=alice's label)
   > |   |   '-- objqZeR5w
   > |   |-- c_EkXYGrmrSit9j_8VjP_-8DgaM=
   > |   |   |-- .label (=label bob \/ alice)
   > |   |   |-- objQaiwuA
   > |   |   '-- objXIBabq
   > |   |       |-- alice -> ../../12to8q3vDp7ApyKEQk2LQ_2nrvs=/objqZeR5w
   > |   |       |-- bob -> ../../oTAUzOt5YoQAyThw89eSgWuig-0=/objtsePnc
   > |   |       '-- messages -> ../../c_EkXYGrmrSit9j_8VjP_-8DgaM=/objQaiwuA
   > |   |-- jX7q5Kb8_G4GsjgNpOHB17kypQo=
   > |   |   |-- .label (=label of the filesystem root)
   > |   |   '-- objTj5JZD
   > |   |       |-- bobOralice -> ../../c_EkXYGrmrSit9j_8VjP_-8DgaM=/objXIBabq
   > |   |       '-- clarice -> ../../wordEFmBzWc7Q6qP-TetDgZaG8A=/objOMh0mj
   > |   |-- oTAUzOt5YoQAyThw89eSgWuig-0=
   > |   |   |-- .label (=bob's label)
   > |   |   |-- objFIQCIm
   > |   |   '-- objtsePnc
   > |   |       '-- secret -> ../../oTAUzOt5YoQAyThw89eSgWuig-0=/objFIQCIm
   > |   '-- wordEFmBzWc7Q6qP-TetDgZaG8A=
   > |       |-- .label (=clarice's label)
   > |       '-- objOMh0mj
   > '-- ROOT -> .obj/jX7q5Kb8_G4GsjgNpOHB17kypQo=/objTj5JZD
   >

   Observe that every directory object contains symbolic links
   pointing to file or directory objects in the store.

   Accessing any file or directory in the \"friendly\" view requires
   a lookup ('lookupObjPath') of the corresponding object. Each
   path is, however, relative to the root. So, for example,
   looking up @\/aliceOrbob\/bob\/@ actually corresponds to looking
   up @\/lioFS\/ROOT\/aliceOrbob\/bob\/@. This prevents untrusted code
   from accessing objects by guessing filenames and further allows
   untrusted code to pick arbitrary filenames (including @.label@,
   @.obj@, etc.).

   Several precautions are taken to keep the filesystem in a working
   state in case of a crash.  The code tries to maintain the invariant
   that any inconsistencies will either be:

     1. temporary files or directories whose names start with the
        \"@#@\" character, or

     2.  dangling symbolic links.

   Both of these inconsistencies can be checked and cleaned up
   locally without examining the whole file system.  The code tries
   to fix up these inconsistencies on-the-fly as it encounters them.
   However, it could possibly leave some stranded temporary
   @.label@ files. 
   TODO can be used to do a full filesystem cleanup (this should only
   be run periodically as it is a costly operation).


-}


module LIO.FS ( evalWithRoot
              -- * Creating and finding (labeled) objects
              , lookupObjPath, lookupObjPathP
              , getObjLabelTCB
              , createFileTCB, createDirectoryTCB
              -- * Labeled 'FilePath'
              , LFilePath, labelOfFilePath
              , unlabelFilePath
              , unlabelFilePathP
              , unlabelFilePathTCB
              -- * Misc helper functions
              , cleanUpPath, stripSlash
              ) where


-- TODO: safe imports
import Prelude hiding (catch)
import Control.Monad (unless)
import LIO.TCB 
import System.FilePath
import System.Posix.Files
import System.Directory
import Control.Exception (Exception)
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C
import Data.Serialize
import Data.IORef
import Data.Functor
import Data.Tuple (swap)
import Data.Foldable (foldlM)
import Data.Typeable
import Data.List (isPrefixOf)
import System.IO.Unsafe
import System.IO
import qualified System.IO.Error as IOError

import qualified Data.ByteString.Base64.URL as Codec
import qualified Data.Digest.Pure.SHA as SHA
import System.Posix.Temp 


--
-- Filesystem root
--


-- | Root of labeled filesystem.
rootDir :: IORef FilePath
{-# NOINLINE rootDir #-}
rootDir = unsafePerformIO $ newIORef undefined

getRootDir :: IO FilePath
getRootDir = readIORef rootDir

-- | Temporary files have the prefix \'#\'.
tmpPrefix :: FilePath
tmpPrefix = "#"

-- | Prefix of object filenames.
objPrefix :: FilePath
objPrefix = "obj"

-- | Shadow directory containing the actual FS objects.
objDir :: FilePath
objDir = ".obj"

-- | We need to serialize the label in each label directory.
labelFile :: FilePath
labelFile = ".label"

-- | Root of the filesystem.
rootLink :: FilePath
rootLink = "ROOT"

-- | Magic file. Last file created when the the filesystem is
-- initially created. If the magic file is invalid, then it is likely
-- that a failure occured in the set up process.
magicFile :: FilePath
magicFile = ".magic"

-- | Content written to magic file. 
magicContent :: B.ByteString
magicContent = B.pack  [ 0x7f, 0x45, 0x4c, 0x46, 0x01
                       , 0x01, 0x01, 0x00, 0x00, 0x00
                       , 0x00, 0x00, 0x00, 0x00, 0x00
                       , 0x00, 0xde, 0xad, 0xbe, 0xef]

-- | Filesystem errors
data FSErr = FSRootCorrupt -- ^ Root structure is corrupt
           | FSRootInvalid -- ^ Root is invalid (must be absolute).
      deriving Typeable

instance Exception FSErr

instance Show FSErr where
  show FSRootCorrupt = "Root structure is corrupt"
  show FSRootInvalid = "Root is invalid (must be absolute)"
  

-- | Same as 'evalLIO', but takes two additional parameters
-- corresponding to the path of the labeled filesystem store and the
-- label of the root. If the labeled filesystem store does not exist,
-- it is created at the specified path with the root having the
-- supplied label.
-- If the filesystem does exist, the supplied label is ignored and thus
-- unnecessary. However, if the root label is not provided and the
-- filesystem has not been initialized, then 'lbot' is used as the
-- root label.
evalWithRoot :: (Serialize l, LabelState l s)
            => FilePath  -- ^ Filesystem root
            -> Maybe l   -- ^ Label of root
            -> LIO l s a -- ^ LIO action
            -> s         -- ^ Initial state
            -> IO (a, l)
evalWithRoot path ml act s = setRootOrInitFS >> evalLIO act s
  where setRootOrInitFS = do
          unless (isAbsolute path) $ throwIO FSRootInvalid
          exists <- doesDirectoryExist path
          let l = maybe lbot id ml
          if exists
            then checkFS path
            else initFS l path

-- | Initialize the filesystem with a given label and root file path.
initFS :: (Label l, Serialize l)
       => l             -- ^ Label of root
       -> FilePath      -- ^ Path to the filesystem root
       -> IO ()
initFS l path = do
  unless (isAbsolute path) $ throwIO FSRootInvalid
  -- Create root of filesystem
  createDirectory path
  -- Create the shadow object store
  createDirectory (path </> objDir)
    `onException` (removeDirectory path)
  -- Set the root filesystem:
  writeIORef rootDir path
  rootObj <- newDirObj l
  linkRoot rootObj
  -- Create magic file:
  B.writeFile (path </> magicFile) magicContent

-- | Check the filesystem structure.
checkFS :: FilePath -> IO ()
checkFS path = do
  -- check that the ROOT link exists
  rootStat <- getSymbolicLinkStatus $ path </> rootLink
  unless (isSymbolicLink rootStat) $ throwIO FSRootCorrupt
  -- find obj dir:
  hasObjDir <- doesDirectoryExist $ path </> objDir
  unless hasObjDir $ throwIO FSRootCorrupt
  -- check magic file:
  magicOK <- (B.readFile (path </> magicFile) >>= \c -> return (c==magicContent))
               `catchIO` (return False)
  unless magicOK $ throwIO FSRootCorrupt
  writeIORef rootDir path


--
-- Objects
-- 

-- | Encoding of a label into a filepath. We need to take the hash
-- of a label (as opposed to e.g. just 64-base encoding of the label)
-- in order to keep the filename lengths to a reasonable size.
encodeLabel :: (Serialize l, Label l) => l -> FilePath
encodeLabel l = enc . SHA.bytestringDigest . SHA.sha1 $ encodeLazy l
  where enc = C.unpack . Codec.encode . B.concat . LB.toChunks

-- | An object can be a directory or a file.
data Object  = DirObj  FilePath         -- ^ Directory object
             | FileObj Handle FilePath  -- ^ File object
             deriving (Eq, Show)

-- | A wrapper for a new, not yet commited, object.
newtype NewObject = New Object
              deriving (Eq, Show)

-- | Create a new directory object with a given label.
newDirObj :: (Serialize l, Label l) => l -> IO NewObject
newDirObj l = newObj l $ \p -> (New . DirObj) <$> mkTmpObjDir p

-- | Create a new file object with a given label and 'IOMode'.
newFileObj :: (Serialize l, Label l) => l -> IOMode -> IO NewObject
newFileObj l m = newObj l $ \p -> (New . uncurry FileObj) <$> mkTmpObjFile m p

-- | Create a new temporary unique object directory. The function retries if
-- there exists an object with the same name but no 'tmpPrefix'.
mkTmpObjDir :: FilePath -> IO FilePath
mkTmpObjDir path = do
  dir <- mkdtemp $ path </> (tmpPrefix ++ objPrefix) 
  exists <- doesDirectoryExist (rmTmpPrefix dir)
  if exists
    then do ignoreErr $ removeDirectory dir
            mkTmpObjDir path
    else return dir


-- | Create a new temporary unique object file. The function retries if
-- there exists an object with the same name but no 'tmpPrefix'.
mkTmpObjFile :: IOMode -> FilePath -> IO (Handle, FilePath)
mkTmpObjFile mode path = do
  res@(file, h) <- mkstemp $ path </> (tmpPrefix ++ objPrefix)
  exists <- doesFileExist (rmTmpPrefix file)
  if exists
    then do hClose h 
            ignoreErr $ removeFile file
            mkTmpObjFile mode path
    else return . swap $ res

-- | Create a new file or directory object (given the make temporary
-- object functino). For example, we can create a new file object as:
--
-- > newDirObj l = newObj l $ \p -> (New . DirObj) <$> mkTmpObjDir p
--
-- or a new directory object as
--
-- > newFileObj l m = newObj l $ \p ->
-- >                    (New . uncurry FileObj) <$> mkTmpObjFile m p)
-- 
newObj :: (Serialize l, Label l)
       => l                           -- ^ Label of object
       -> (FilePath -> IO NewObject)  -- ^ Object creation function
       -> IO NewObject
newObj l mkFunc = getLabelDir l >>= mkFunc

-- | Create the label directory (and corresponding 'labelFile') in 'objDir'.
getLabelDir :: (Serialize l, Label l) => l -> IO FilePath
getLabelDir l = do
  oRoot <- (</> objDir) <$> getRootDir
  let dir = oRoot </> encodeLabel l
  createDirectoryIfMissing True dir
  createLabelFileIfMissing dir l
  return dir

-- | This function creates a 'labelFile' for the given label
-- directory.  It also verifies that the label in the file is the
-- same as the given label (as another thread might have created the
-- file, or a previous write might have corrupted the file). If the
-- file is invalid, it removes the existing label file and writes
-- the given label. An assumption made here is that there is a 1-to-1
-- correspondence between a label and its encoding (i.e., 'encodeLabel'
-- is a bijection).
createLabelFileIfMissing :: (Serialize l, Label l) => FilePath -> l ->  IO ()
createLabelFileIfMissing dir l = do
  let file = dir </> labelFile
  exists <- doesFileExist file
  unless exists $ createLabelFile file
  -- It's possible for another thread to have created the file, and so
  -- we need to make sure that we agree on the label:
  valid <- checkLabelFile file
  unless valid $ (ignoreErr $ removeFile file) >> createLabelFileIfMissing dir l
    where createLabelFile file = do
            (h,f) <- mkTmpObjFile WriteMode dir
            C.hPutStr h (encode l)
            hClose h
            rename f file `E.catch` (\e -> 
              removeFile f >> unless (IOError.isAlreadyExistsError e) (throwIO e))
          checkLabelFile file = do
            c <- B.readFile file
            case decode c of
              Left _   -> return False
              Right l' -> return (l == l')


-- | Link the 'rootLink' to an object directory.
linkRoot :: NewObject -> IO ()
linkRoot newO@(New o) = do
  root <- getRootDir
  newObjPath <- case o of
                  DirObj p -> return p
                  _        -> throwIO . userError $ "Root must be a directory."
  let objPath = rmTmpPrefix newObjPath
  let name = root </> rootLink
  createSymbolicLink (objPath `rmPrefixDir` root) name
    `onException` (cleanUpNewObj newO)
  rename newObjPath objPath `onException` (do ignoreErr $ removeFile name
                                              cleanUpNewObj newO)

-- | Link without returning object.
linkObj_ :: FilePath -> NewObject -> IO ()
linkObj_ f n = linkObj f n >> return ()


-- | Link a filepath to an object.
linkObj :: FilePath -> NewObject -> IO Object
linkObj name' newO@(New o) = do
  root <- getRootDir
  let newObjPath = case o of
                     DirObj p    -> p
                     FileObj _ p -> p
      objPath = rmTmpPrefix newObjPath
  let name = root </>  rootLink </> name'
      relObjPath = (".." </> ".." </> (objPath `rmPrefixDir` (root </> objDir)))
  createSymbolicLink relObjPath name `onException` (cleanUpNewObj newO)
  rename newObjPath objPath `onException` (do ignoreErr $ removeFile name
                                              cleanUpNewObj newO)
  return $ case o of
             DirObj _    -> DirObj objPath
             FileObj h _ -> FileObj h objPath

-- | It's possible that either a program crashed before renaming a
-- 'NewObject' into a 'Object', or that another thread is calling
-- 'linkObj' and for some reason is being slow between the
-- 'createSymbolicLink' and 'rename' calls.  Either way it should be
-- fine for us just to 'rename' the 'NewObject', because the link to
-- the object would not exist if the 'NewObject' were not ready to be
-- renamed. This function is primarily used when traversing the
-- filesystem tree with 'pathTaintTCB'.
fixObjLink :: FilePath -> IO ()
fixObjLink f = do
  exists <- catchIO (getFileStatus f >> return True) (return False)
  unless exists $ ignoreErr $ rename (tmpPrefix ++ f) f

-- | Clean up a newly created object. If the object is a temporary
-- directory, remove it. If it's a file, close the handle, and remove
-- the file.
cleanUpNewObj :: NewObject -> IO ()
cleanUpNewObj (New o) = ignoreErr $ case o of 
                                      DirObj p -> removeDirectory p
                                      FileObj h p -> hClose h >> removeFile p

-- | 'Label' associated with a 'FilePath'.
newtype LFilePath l = LFilePathTCB (Labeled l FilePath)

-- | Get the label of a labeled  filepath.
labelOfFilePath :: Label l =>  LFilePath l -> l
labelOfFilePath (LFilePathTCB x) = labelOf x

-- | Trusted version of 'unlabelFilePath' that ignores IFC.
unlabelFilePathTCB :: (Label l) => LFilePath l -> FilePath
unlabelFilePathTCB (LFilePathTCB l) = unlabelTCB l

-- | Same as 'unlabelFilePath' but uses privileges to unlabel the
-- filepath.
unlabelFilePathP :: (Priv l p, LabelState l s)
                 => p -> LFilePath l -> LIO l s FilePath
unlabelFilePathP p (LFilePathTCB l) = unlabelP p l

-- | Unlabel a filepath. If the path corresponds to a directory, you
-- can now get the contents of the directory; if it's a file, you can
-- open the file.
unlabelFilePath :: LabelState l s
                 => LFilePath l -> LIO l s FilePath
unlabelFilePath = unlabelFilePathP NoPrivs

-- | Given a pathname (forced to be relative to the root of the
-- labeled filesystem), find the path to the corresponding object.
-- The current label is raised to reflect all the directories traversed.
-- Note that if the object does not exist an exception will be thrown;
-- the label of the exception will be the join of all the directory labels 
-- up to the lookup failure.
--
-- Additionally, this function cleans up the
-- path before doing the lookup, so e.g., path @/foo/bar/..@ will
-- first be rewritten to @/foo@ and thus no traversal to @bar@.
-- Note that this is a more permissive behavior than forcing the read
-- of @..@ from @bar@.
lookupObjPath :: (LabelState l s, Serialize l)
              => FilePath  -- ^ Path to object
              -> LIO l s (LFilePath l)
lookupObjPath = lookupObjPathP NoPrivs

-- | Same as 'lookupObjPath' but takes an additional privilege object
-- that is exercised when raising the current label.
lookupObjPathP :: (Priv l p, LabelState l s, Serialize l)
               => p         -- ^ Privilege 
               -> FilePath  -- ^ Path to object
               -> LIO l s (LFilePath l)
lookupObjPathP p f' = do
  f <- cleanUpPath f'
  root <- ioTCB $ getRootDir
  pathTaintTCB p root rootLink
  let dirs = splitDirectories f
  dir <- foldlM (\a b ->  pathTaintTCB p a b >> return (a </> b))
                (root </> rootLink)
                (safeinit dirs)
  let objPath = (dir </> safelast dirs)
  l <- getObjLabelTCB objPath
  return . LFilePathTCB $ labelTCB l objPath
    where safelast [] = ""
          safelast x  = last x


-- | Read the label file of an object. Note that because the format
-- of the supplied path is not checked this function is considered to
-- be in the @TCB@.
getObjLabelTCB :: (Serialize l, LabelState l s) => FilePath -> LIO l s l
getObjLabelTCB objPath = rtioTCB $ do
  root <- getRootDir
  symLink <- readSymbolicLink objPath
  let absObjPath = root </> objDir </>
                      ((symLink `rmPrefixDir` (".." </> ".."))
                                `rmPrefixDir` objDir)
  lS  <- C.readFile $ (takeDirectory absObjPath) </> labelFile
  case decode lS of
    Left _  -> throwIO . userError $ "Invalid label file."
    Right l ->  return l 

-- | Given a privilege, root-path prefix and a directory/file within
-- the prefix, read the 'labelFile' of the root-path prefix, raising
-- the current label to reflect this. This function is used to taint 
-- a process that traverses a filesystem tree. The function is @TCB@
-- because we do not check any properties of the root -- it is
-- primarily used by 'lookupObjPathP'.
pathTaintTCB :: (Serialize l, Priv l p, LabelState l s)
              => p -> FilePath -> FilePath -> LIO l s ()
pathTaintTCB p root f' = do
  let f = stripSlash f'
  rootL <- rtioTCB $ readSymbolicLink (root </> f)
  let objPath = root </> rootL
  ioTCB $ fixObjLink objPath
  lS <- rtioTCB $ C.readFile (objPath </> ".." </> labelFile)
  case decode lS of
    Left _ -> throwIO . userError $ "Invalid label file."
    Right l -> taintP p l

-- | Remove any 'pathSeparator's from the front of a file path.
stripSlash :: FilePath -> FilePath 
stripSlash [] = []
stripSlash xx@(x:xs) | x == pathSeparator = stripSlash xs
                     | otherwise          = xx

-- | Cleanup a file path, if it starts out with a @..@, we consider
-- this invalid as it can be used explore parts of the filesystem
-- that should otherwise be unaccessible. Similarly, we remove any @.@
-- from the path.
cleanUpPath :: LabelState l s => FilePath -> LIO l s FilePath 
cleanUpPath f = rtioTCB $ doit $ splitDirectories . normalise . stripSlash $ f
  where doit []          = return []
        doit ("..":[])   = return "/"
        doit ("..":_)    = throwIO $ IOError.mkIOError IOError.doesNotExistErrorType
                              "Illegal filename" Nothing (Just ".." )
        doit (_:"..":xs) = doit xs
        doit (".":xs)    = doit xs
        doit (x:xs)      = (x </>) <$> doit xs
  

--
-- Creating and linking directory and file objects
--

-- | Create a directory object with the given label and link the
-- supplied path to the object.
createDirectoryTCB :: (Serialize l, Label l) => l -> FilePath -> IO ()
createDirectoryTCB l p = do
  obj <- newDirObj l
  linkObj_ p obj

-- | Create a file object with the given label and link the
-- supplied path to the object. The handle to the file is returned.
createFileTCB :: (Serialize l, Label l) => l -> FilePath -> IOMode -> IO Handle
createFileTCB l p m = do
  obj <- newFileObj l m
  (FileObj h _) <- linkObj p obj
  return h



--
-- Helper functions
--

-- | Remove the prefix from a list (usually 'FilePath').
rmPrefix :: Eq a => [a] -- ^ String
                 -> [a] -- ^ Prefix
                 -> [a]
rmPrefix s pre = if pre `isPrefixOf` s
                   then drop (length pre) s
                   else s

-- | Remove the 'tmpPrefix' prefix from a file or directory.
rmTmpPrefix :: FilePath -> FilePath
rmTmpPrefix path =
  let ds = splitDirectories path
  in (joinPath $ safeinit ds) 
     </> ((last ds) `rmPrefix` tmpPrefix)


-- | @rmPrefixDir path prefix@ removes @prefix@ from @path@.
rmPrefixDir :: FilePath -> FilePath -> FilePath
rmPrefixDir path prefix = 
  if prefix `isPrefixOf` path
    then let p = splitDirectories path
             x = splitDirectories prefix
         in joinPath $ drop (length x) p
    else path

-- | Ignore 'IOException's.
ignoreErr :: IO () -> IO ()
ignoreErr m = catchIO m (return ())

-- | Same as 'catch', but only catches 'IOException's.
catchIO :: IO a -> IO a -> IO a
catchIO a h = E.catch a ((const :: a -> E.IOException -> a) h)

-- | Same as 'init', but does not crash on empty list.
safeinit :: [a] -> [a]
safeinit [] = []
safeinit x  = init x
