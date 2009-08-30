{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
-- {-# OPTIONS_GHC -fglasgow-exts #-}

module LIO.IOTCB {- (
                   LIORef, newLIORef, labelOfLIORef
                 , readLIORef, writeLIORef, atomicModifyLIORef
                 , IOMode, IsHandle(..)
                 ) -}
    where

import LIO.Armor
import LIO.TCB
import LIO.TmpFile

import Prelude hiding (catch)
import Control.Exception (throwIO, catch, SomeException(..)
                         , Exception(..), IOException(..))
import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IORef
import Data.Typeable (Typeable)
import Data.Word (Word8)
import qualified System.Directory as IO
import System.FilePath (FilePath(..), (</>)
                       , isPathSeparator, replaceFileName
                       , splitDirectories, joinPath, normalise)
import System.IO (IOMode(..), FilePath(..), stderr)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import System.Posix.Directory
import System.Posix.Files (rename, getFileStatus
                          , createSymbolicLink, readSymbolicLink)
import System.Posix.IO
import System.Time (ClockTime(..), getClockTime)

import Data.Digest.Pure.SHA
import qualified System.IO.Cautious as CIO


--
-- LIOref -- labeled IOref
--

data LIORef l a = LIORefTCB l (IORef a)

newLIORef :: (Label l, Typeable s) => l -> a -> LIO l s (LIORef l a)
newLIORef l a = do
  guardio l
  ior <- ioTCB $ newIORef a
  return $ LIORefTCB l ior

labelOfLIORef :: (Label l) => LIORef l a -> l
labelOfLIORef (LIORefTCB l _) = l

readLIORef :: (Label l, Typeable s) => LIORef l a -> LIO l s a
readLIORef (LIORefTCB l r) = do
  taintio l
  val <- ioTCB $ readIORef r
  return val

writeLIORef :: (Label l, Typeable s) => LIORef l a -> a -> LIO l s ()
writeLIORef (LIORefTCB l r) a = do
  guardio l
  ioTCB $ writeIORef r a

atomicModifyLIORef :: (Label l, Typeable s) =>
                      LIORef l a -> (a -> (a, b)) -> LIO l s b
atomicModifyLIORef (LIORefTCB l r) f = do
  guardio l
  ioTCB $ atomicModifyIORef r f

--
-- File operations
--

class IsHandleOpen h m where
    getDirectoryContents :: FilePath -> m [FilePath]
    createDirectory      :: FilePath -> m ()
    openFile             :: FilePath -> IOMode -> m h
    hClose               :: h -> m ()

instance IsHandleOpen IO.Handle IO where
    getDirectoryContents = IO.getDirectoryContents
    createDirectory      = IO.createDirectory
    openFile             = IO.openBinaryFile
    hClose               = IO.hClose

class (IsHandleOpen h m) => IsHandle h b m where
    hGet            :: h -> Int -> m b
    hGetNonBlocking :: h -> Int -> m b
    hPut            :: h -> b -> m ()
    hPutStrLn       :: h -> b -> m ()

instance IsHandle IO.Handle L.ByteString IO where
    hGet            = L.hGet
    hGetNonBlocking = L.hGetNonBlocking
    hPut            = L.hPut
    hPutStrLn h s   = L.hPut h $ L.append s $ L.singleton 0xa

data LHandle l h = LHandleTCB l h

instance (Label l, IsHandleOpen (LHandle l h) (LIO l s), IsHandle h b IO)
    => IsHandle (LHandle l h) b (LIO l s) where
    hGet (LHandleTCB l h) n      = guardio l >> rtioTCB (hGet h n)
    hGetNonBlocking (LHandleTCB l h) n =
                                 guardio l >> rtioTCB (hGetNonBlocking h n)
    hPut (LHandleTCB l h) s      = guardio l >> rtioTCB (hPut h s)
    hPutStrLn (LHandleTCB l h) s = guardio l >> rtioTCB (hPutStrLn h s)

instance (Label l, IsHandleOpen h IO) =>
    IsHandleOpen (LHandle l h) (LIO l s) where
        getDirectoryContents    = undefined
        createDirectory         = undefined
        openFile                = undefined
        hClose (LHandleTCB l h) = guardio l >> rtioTCB (hClose h)


hlabelOf                  :: (Label l) => LHandle l h -> l
hlabelOf (LHandleTCB l h) = l

--
-- Labeled storage
--

data LIOerr
    = LioCorruptLabel String String -- ^File Containing Label is Corrupt
      deriving (Show, Typeable)
instance Exception LIOerr
            

labelStr2Path   :: String -> FilePath
labelStr2Path l = case armor32 $ bytestringDigest $ sha224 $ LC.pack $ l of
                    c1:c2:c3:rest -> ((c1:[]) </> (c2:[]) </> (c3:[]) </> rest)
labeledNode2Root = ".." </> ".." </> ".." </> "."

strictReadFile   :: FilePath -> IO LC.ByteString
strictReadFile f = IO.withFile f ReadMode readit
    where readit h = do
            size <- IO.hFileSize h
            LC.hGet h $ fromInteger size

labelfname = ".label"
getLabelDir   :: Label l => l -> IO String
getLabelDir l =
    let label = show l
        path = labelStr2Path label
        file = path </> labelfname
        correct = LC.pack $ label
        checkfile = do
          contents <- strictReadFile file
          unless (contents == correct) $ do
                 IO.hPutStrLn stderr (file ++ " should contain:\n" ++ label)
                 throwIO (LioCorruptLabel file $ LC.unpack correct)
        nosuch e = if IO.isDoesNotExistError e
                   then do let tpath = path ++ "~"
                               tfile = tpath </> labelfname
                           IO.createDirectoryIfMissing True tpath
                           putStrLn $ "creating " ++ tfile
                           CIO.writeFileL tfile correct
                           rename tpath path
                   else throwIO e
    in do checkfile `catch` nosuch
          return path

catchIO     :: IO a -> IO a -> IO a
catchIO a h = catch a ((const :: a -> IOException -> a) h)

-- | Create file or directory in appropriate directory for a given
-- label.  Node gets created with an extra ~ appended.
lmknod     :: (Label l) => l
           -- ^Label for the new node
           -> (FilePath -> FilePath -> IO (a, FilePath))
           -- ^Either 'mkTmpDir' or 'mkTmpFile' with curried 'IOMode'
           -> IO (a, FilePath)
           -- ^Returns file handle or () and destination path
lmknod l f = do
  d <- getLabelDir l
  (a, p) <- f d "~"
  let p' = init p
  exists <- catchIO (getFileStatus p' >> return True) (return False)
  if not exists
    then return (a, p')
    else do
      IO.hPutStrLn stderr $ "lmknod: file " ++ p' ++ " already exists." -- XXX
      IO.removeFile p `catchIO` (IO.removeDirectory p `catchIO` return ())
      lmknod l f

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
      doit d s = joinPath (replicate (length s) "../" ++ d)

mklink p name = do
  createSymbolicLink (p `makeRelativeTo` name) name
  rename (p ++ "~") p

lmkdir1 :: (Label l) => l -> FilePath -> IO ()
lmkdir1 l name = do
  ((), p) <- lmknod l mkTmpDir
  mklink p name

lcreat1 :: (Label l) => l -> IOMode -> FilePath -> IO IO.Handle
lcreat1 l m name = do
  (h, p) <- lmknod l (mkTmpFile m)
  mklink p name
  return h

stripdotdot ('.':'.':'/':s) = stripdotdot s
stripdotdot s               = s


namei :: forall l p s. (Priv l p) =>
         p -> FilePath -> LIO l s (FilePath, FilePath)
namei priv path = do
  lookup "root" "." (stripslash $ splitDirectories path)
    where
      stripslash (('/':_):t) = t
      stripslash t = t

      lookup d p (cn1:[])  = do
        nd <- fmap (`makeRelativeTo` p) $ rtioTCB $ readSymbolicLink d
        taintcn nd
        return (nd, cn1)
      lookup d p (cn1:cns) = do
        nd <- fmap (`makeRelativeTo` p) $ rtioTCB $ readSymbolicLink d
        taintcn nd
        lookup (nd </> cn1) labeledNode2Root cns

      taintcn :: FilePath -> LIO l s ()
      taintcn path = do l <- label; ptaintio priv l
          where
            lpath = replaceFileName path labelfname
            getlabel = do
              labelstr <- ioTCB $ readFile lpath
              return (read labelstr :: l)
            corrupted   :: SomeException -> LIO l s l
            corrupted e = throwL $ LioCorruptLabel lpath "<unknown>"
            label = getlabel `catchL` corrupted
                                 

ls = "labeledStorage"
lsinitTCB   :: (Label l) => l -> LIO l s ()
lsinitTCB l = rtioTCB $ changeWorkingDirectory ls `catchIO` setup
    where
      setup = do
        IO.createDirectoryIfMissing True ls
        changeWorkingDirectory ls
        lmkdir1 l "root"


--
-- Crap
--


lgetLine :: (Label l, Typeable s) => LIO l s String
lgetLine = ioTCB getLine
lputStr x = ioTCB $ putStr x
lputStrLn x = ioTCB $ putStrLn x

