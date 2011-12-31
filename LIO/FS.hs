{-# LANGUAGE ScopedTypeVariables #-}
module LIO.FS where


import Prelude hiding (catch)
import Control.Monad (unless)
import LIO.TCB 
import LIO.MonadCatch
import System.FilePath
import System.Posix.Files
import System.Directory
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Serialize
import Data.IORef
import Data.Functor
import Data.Foldable (foldlM)
import Data.List (isPrefixOf, isSuffixOf, findIndex)
import System.IO.Unsafe
import System.IO
import qualified System.IO.Error as IOError

import qualified LIO.Armor as Codec
import qualified Data.Digest.Pure.SHA as SHA
import qualified LIO.TmpFile as Tmp


--
-- Filesystem root
--


-- | Root of labeled filesystem.
rootDir :: IORef FilePath
{-# NOINLINE rootDir #-}
rootDir = unsafePerformIO $ newIORef undefined

getRootDir :: IO FilePath
getRootDir = readIORef rootDir

-- | Extension of temporary file.
tmpExt :: FilePath
tmpExt = "~"


-- | Shadow directory containing the actual FS objects.
objDir :: FilePath
objDir = ".obj"

-- | We need to serialize the label in each label directory.
labelFile :: FilePath
labelFile = ".label"


-- | Root of the file system.
rootFile :: FilePath
rootFile = "ROOT"

-- | Same as 'evalLIO', but takes an additional parameter
-- corresponding to the path of the labeled filesystem. If the
-- labeled filesystem does not exist, it is created at the specified
-- path with the root having the label bottom ('lbot').
evalWithRoot :: (Serialize l, LabelState l s)
            => FilePath -> LIO l s a -> s -> IO (a, l)
evalWithRoot path act s = (flip evalLIO) s $ do
  l <- getLabel
  ioTCB $ initFS l path
  --ioTCB $ setOrMakeRoot l
  act
  {-
    where throwIOStr = throwIO . userError
          setOrMakeRoot l = do
            unless (isAbsolute path) $ throwIOStr "Root path must be absolute."
            exists <- doesDirectoryExist path
            if exists
              then do hasLabel <- doesFileExist $ path </> labelFile
                      unless hasLabel $ throwIOStr "No label file found."
              else makeRoot l path
            writeIORef rootDir path
          tryReadLabel _ path = do
             mL <- decode <$> B.readFile path
             case mL of
               Left _ -> throwIO . userError $ "Could not read label file"
               Right l -> return l
               -}


-- | Initialize the filesystem with a given label and root file path.
initFS :: (Label l, Serialize l)
       => l             -- ^ Label of root
       -> FilePath      -- ^ Path to the filesystem root
       -> IO ()
initFS l path = do
  unless (isAbsolute path) $ throwIOStr $ "Root path must be absolute."
  -- Create root of filesystem
  createDirectory path
  -- Create the shadow object store
  createDirectory (path </> objDir)
    `onException` (removeDirectory path)
  -- Set the root filesystem:
  writeIORef rootDir path
  rootObj <- newDirObj l
  linkRoot rootObj
  return ()
    where throwIOStr = throwIO . userError


--
-- Objects
-- 

-- | Encoding of a label into a filepath. We need to take the hash
-- of a label (as opposed to e.g. just 64-base encoding of the label)
-- in order to keep the filename lengths to a reasonable size.
encodeLabel :: (Serialize l, Label l) => l -> FilePath
encodeLabel l = Codec.armor32 . SHA.bytestringDigest . SHA.sha1 $ encodeLazy l

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

-- | Remove the suffix (usually 'tmpExt') from a filename.
rmSuffix :: String -> String -> String
rmSuffix p e = if e `isSuffixOf` p
  then take (length p - length e) p
  else p

-- | Create a new temporary unique object directory. The function retries if
-- there exists an object with the same name but no 'tmpExt' suffix.
mkTmpObjDir :: FilePath -> IO FilePath
mkTmpObjDir path = do
  dir <- Tmp.mkTmpDir' path tmpExt
  exists <- doesDirectoryExist (dir `rmSuffix` tmpExt)
  if exists
    then do ignoreErr $ removeDirectory dir
            mkTmpObjDir path
    else return dir


-- | Create a new temporary unique object file. The function retries if
-- there exists an object with the same name but no 'tmpExt' suffix.
mkTmpObjFile :: IOMode -> FilePath -> IO (Handle, FilePath)
mkTmpObjFile mode path = do
  res@(h,file) <- Tmp.mkTmpFile mode path tmpExt
  exists <- doesFileExist (file `rmSuffix` tmpExt)
  if exists
    then do hClose h 
            ignoreErr $ removeFile file
            mkTmpObjFile mode path
    else return res

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


-- | Link the 'rootFile' to an object directory.
linkRoot :: NewObject -> IO ()
linkRoot newO@(New o) = do
  root <- getRootDir
  newObjPath <- case o of
                  DirObj p -> return p
                  _        -> throwIO . userError $ "Root must be a directory."
  let objPath = newObjPath `rmSuffix` tmpExt
  let name = root </> rootFile
  createSymbolicLink (objPath `rmPrefix` root) name `onException` (cleanUpNewObj newO)
  rename newObjPath objPath `onException` (do ignoreErr $ removeFile name
                                              cleanUpNewObj newO)

-- | Link without returning object.
linkObj_ :: FilePath -> NewObject -> IO ()
linkObj_ f n = linkObj f n >> return ()


-- | Link a file path to an object.
linkObj :: FilePath -> NewObject -> IO Object
linkObj name' newO@(New o) = do
  root <- getRootDir
  let newObjPath = case o of
           DirObj p    -> p
           FileObj h p -> p
      objPath = newObjPath `rmSuffix` tmpExt
  link <- readSymbolicLink (root </> rootFile)
  let name = root </>  rootFile </> name'
      relObjPath = (".." </> ".." </> (objPath `rmPrefix` (root </> objDir)))
  createSymbolicLink relObjPath name `onException` (cleanUpNewObj newO)
  rename newObjPath objPath `onException` (do ignoreErr $ removeFile name
                                              cleanUpNewObj newO)
  return $ case o of
             DirObj _    -> DirObj objPath
             FileObj h _ -> FileObj h objPath

-- | Clean up a newly created object. If the object is a temporary
-- directory, remove it. If it's a file, close the handle, and remove
-- the file.
cleanUpNewObj :: NewObject -> IO ()
cleanUpNewObj (New o) = ignoreErr $ case o of 
                                      DirObj p -> removeDirectory p
                                      FileObj h p -> hClose h >> removeFile p

-- | Labeled file path.
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
-- labeled file system), find the path to the corresponding object.
-- The current label is raised to reflect all the directories traversed.
-- Note that if the object does not exist an exception will be thrown;
-- the label of the exception will be the label of the last directory
-- before lookup failure. Additionally, this function cleans up the
-- path before doing the lookup, so e.g., a path @/foo/bar/..@ will
-- first be rewritten to @/foo@ and thus not reflect a traversal to @bar@.
-- Note that this is a more permissive behavior than forcing the read
-- of @..@ from @bar@.
lookupObjPath :: (LabelState l s, Serialize l)
              => FilePath  -- ^ Path to object
              -> LIO l s (LFilePath l)
lookupObjPath = lookupObjPathP NoPrivs

-- | Same as 'lookupObjPath' but takes an additionally privilege object
-- exercised when tainting the process.
lookupObjPathP :: (Priv l p, LabelState l s, Serialize l)
               => p         -- ^ Privilege 
               -> FilePath  -- ^ Path to object
               -> LIO l s (LFilePath l)
lookupObjPathP p f' = do
  f <- cleanUpPath f'
  root <- ioTCB $ getRootDir
  pathTaintTCB p root rootFile
  let dirs = splitDirectories f
  dir <- foldlM (\a b ->  pathTaintTCB p a b >> return (a </> b))
                (root </> rootFile)
                (safeinit dirs)
  let objPath = (dir </> safelast dirs)
  l <- getObjLabelTCB objPath
  return . LFilePathTCB $ labelTCB l objPath
  {-
  stat <- rtioTCB $ getFileStatus objPath
  lS <- rtioTCB $ C.readFile (objPath </> ".." </> labelFile)
  case decode lS of
    Left _ -> throwIO . userError $ "Invalid label file."
    Right l ->  return . LFilePathTCB $ labelTCB l objPath
  -}
  where safeinit [] = []
        safeinit x  = init x
        safelast [] = []
        safelast x  = last x

-- | Read the label file of an object. Note that because the format
-- of the supplied path is not checked this function is considered to
-- be in the @TCB@.
getObjLabelTCB :: (Serialize l, LabelState l s) => FilePath -> LIO l s l
getObjLabelTCB objPath = rtioTCB $ do
  root <- getRootDir
  symLink <- readSymbolicLink objPath
  let absObjPath = root </> objDir </>
                      ((symLink `rmPrefix` (".." </> "..")) `rmPrefix` objDir)
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
  lS <- rtioTCB $ C.readFile (objPath </> ".." </> labelFile)
  case decode lS of
    Left _ -> throwIO . userError $ "Invalid label file."
    Right l -> taintP p l

-- | Remove the slashes form the front of a path.
stripSlash :: FilePath -> FilePath 
stripSlash [] = []
stripSlash ('/':xs) = stripSlash xs
stripSlash x = x

-- | Cleanup a file path, if it starts out with a @..@, we consider
-- this invalid as it can be used explore parts of the file system
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


-- | @rmPrefix path prefix@ removes @prefix@ from @path@.
rmPrefix :: FilePath -> FilePath -> FilePath
rmPrefix path prefix = 
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






















