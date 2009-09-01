{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module creates new files and directories with unique names.
-- Its functionality is similary to C's mkstemp() and mkdtemp()
-- functions.
module LIO.TmpFile (-- * The high level interface
                    mkTmpFile
                   , mkTmpDir
                   , mkTmpDir'
                   -- * Some lower-level helper functions
                   , mkTmp, openFileExclusive
                   -- * Functions for generating unique names
                   , tmpName, nextTmpName, serializele, unserializele
                   -- * For flushing temp files before rename
                   , hSync
                   )where

import LIO.Armor

import Prelude hiding (catch)
import Control.Exception (throwIO, catch, Exception(..))
import qualified Control.Exception as IO
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import Foreign.C.Error
import Foreign.C.Types
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.FilePath (FilePath(..), (</>))
import System.Posix.IO (OpenMode(..), OpenFileFlags(..)
                       , defaultFileFlags , openFd
                       , fdToHandle, handleToFd)
import System.Posix.Types (Fd(..), FileMode)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import System.Time (ClockTime(..), getClockTime)

foreign import ccall "unistd.h fsync" c_fsync :: CInt -> IO CInt

--
-- Temporary file name based on time in 1/16 of a microsecond, then
-- step until unused file name found.
--

-- | Serialize an Integer into an array of bytes, in little-endian
-- order.
serializele :: Int              -- ^ Minimum number of bytes to return
            -> Integer          -- ^ The Integer to serialize
            -> [Word8]
serializele n i | n <= 0 && i <= 0 = []
serializele n i = (fromInteger i):serializele (n - 1) (i `shiftR` 8)

-- | Take an array of bytes containing an Integer serialized in
-- little-endian order, and return the Integer.
unserializele       :: [Word8] -> Integer
unserializele []    = 0
unserializele (c:s) = (fromIntegral c) .|. (unserializele s `shiftL` 8)

-- | Return a temorary file name, based on the value of the current
-- time of day clock.
tmpName :: IO String
tmpName = do
  (TOD sec psec) <- getClockTime
  return $ armor32 $ L.pack $
          serializele 3 (psec `shiftR` 16) ++ serializele 4 sec

-- | When the file name returned by 'tmpName' already exists,
-- @nextTmpName@ modifies the file name to generate a new one.
nextTmpName :: String -> String
nextTmpName s =
    let val = unserializele $ L.unpack $ dearmor32 s
    in armor32 $ L.pack $ serializele 7 (1 + val)

-- | Opens a file in exclusive mode, throwing AlreadyExistsError if
-- the file name is already in use.
openFileExclusive     :: IO.IOMode -> FilePath -> IO IO.Handle
openFileExclusive m p = do
  let dom = defaultFileFlags { exclusive = True }
      (om, fm) = case m of
                   IO.WriteMode     -> (WriteOnly, dom)
                   IO.AppendMode    -> (WriteOnly, dom { append = True })
                   IO.ReadWriteMode -> (ReadWrite, dom)
  fd <- openFd p om (Just $ toEnum 0o666) fm
  fdToHandle fd

-- | Executes a function on temporary file names until the function
-- does not throw AlreadyExistsError.  For example, 'mkTmpFile' is
-- defined as:
--
-- > mkTmpFile m d s = mkTmp (openFileExclusive m) d s
--
mkTmp       :: (FilePath -> IO a) -- ^The function to execute (@f@)
            -> FilePath           -- ^Directory to prepend to temp file names
            -> String             -- ^Suffix for new file name
            -> IO (a, FilePath)   -- ^The result of @f@ and the
                                  -- FilePath on which it finally
                                  -- succeeded.
mkTmp f dir suffix = tmpName >>= loop
    where
      ff n = case dir </> n of path -> do a <- f path; return (a, path)
      loop name = ff (name ++ suffix) `catch` reloop name
      reloop name e = if IO.isAlreadyExistsError e
                      then loop $ nextTmpName name
                      else throwIO e


-- | Creates a new file with a unique name in a particular directory
mkTmpFile :: IO.IOMode          -- ^@WriteMode@, @AppendMode@, or
                                -- @ReadWriteMode@ (It is an error to
                                -- use @ReadMode@.)
          -> FilePath           -- ^Directory in which to create file
          -> String             -- ^Suffix for new file name
          -> IO (IO.Handle, FilePath) -- ^Returns open handle to new
                                      -- file, along with pathname of
                                      -- new file
mkTmpFile m d s = mkTmp (openFileExclusive m) d s

-- | Creates a new subdirectory with uniqe file name.  Returns the
-- pathname of the new directory as the second element of a pair, just
-- for consistency with the interface to 'mkTmpFile'.  See
-- `mkTmpDir'` if you don't want this behavior.
mkTmpDir     :: FilePath -- ^Directory in which to create subdirectory
             -> String   -- ^Suffix to append to new directory name
             -> IO ((), FilePath) -- ^Returns full path to new directory
mkTmpDir d s = mkTmp createDirectory d s

-- | Like 'mkTmpDir', but just returns the pathname of the new directory.
mkTmpDir'     :: FilePath -- ^Directory in which to create subdirectory
              -> String   -- ^Suffix to append to new directory name
              -> IO FilePath    -- ^Returns full path to new directory
mkTmpDir' d s = fmap snd $ mkTmpDir d s

-- | Flushes a Handle to disk with fsync()
hSync   :: IO.Handle -> IO ()
hSync h = do
  IO.hFlush h
  (Fd fd) <- handleToFd h
  throwErrnoPathIfMinus1_ "fsync" (show h) (c_fsync fd)
