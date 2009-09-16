
-- module LIO.DCLabel.PrivStore where
module Main where

import Prelude hiding (readFile, writeFile, catch)
import Control.Monad
import Control.Exception hiding (throwIO, catch, onException
                                , bracket, block, unblock)
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Word (Word8, Word16)
import System.Directory
import System.FilePath
import Text.Printf
import System.Time (ClockTime(..), getClockTime)

import Data.Digest.Pure.SHA

import LIO.Armor
import LIO.LIO
import LIO.DCLabel
import LIO.TCB
import LIO.TmpFile

data PrivStore = PS { psKey :: L.ByteString
                    , psPrefix :: FilePath
                    }

getRandomBytes :: IO L.ByteString
getRandomBytes = foldr orElse (error "no random device") $
                 map tryFile ["/dev/urandom", "/dev/random"]
    where
      len = 28
      orElse x y = catch x (\(SomeException _) -> unblock y)
      tryFile f = do
        s <- bracket (openFile f ReadMode) hClose $ flip L.hGet len
        if L.length s == fromIntegral len
          then return s
          else error $ f ++ ": short read"

defaultPrefix :: FilePath
defaultPrefix = "ps"
keyfile :: FilePath
keyfile = "key"

princ2FilePath :: PrivStore -> Principal -> FilePath
princ2FilePath ps (Principal p) = psPrefix ps </> (outerEncode $ innerEncode p)
    where
      outerEncode s@('.':_) = '\\' : s
      outerEncode s = s
      innerEncode (c:s)
          | c == '/' || c == '\\' || ord c < 0x20 || ord c >= 0x7f =
              printf "\\x%x\\&%s" (ord c) (innerEncode s)
      innerEncode (c:s) = c:innerEncode s
      innerEncode [] = []

princ2ascii :: Principal -> [Word8]
princ2ascii (Principal p) = strip $ encode p
    where
      strip []    = []
      strip (c:s) = toEnum (ord c) : strip s
      encode (c:s) | c == '/' || c == '\\' || ord c < 0x20 || ord c >= 0x7f =
                       printf "\\x%x\\&%s" (ord c) (encode s)
      encode (c:s)     = c:encode s
      encode []        = []

-- | Returns 0, but should look up a 16-bit generation number in file
-- system
princGen     :: PrivStore -> Principal -> IO Word16
princGen _ _ = return 0

picklePrivTCB :: PrivStore
              -> Principal      -- ^ Principal to pickle
              -> Integer        -- ^ Absolute expiration time
              -> IO String      -- ^ The result
picklePrivTCB ps princ expire = do
  genp <- liftM (L.pack . serializele 2 . toInteger) (princGen ps princ)
  let expp = L.pack $ serializele 5 expire
      princp = L.pack $ princ2ascii princ
      macIn = expp `L.append` genp `L.append` princp
      macOut = bytestringDigest $ hmacSha224 (psKey ps) macIn
      out = L.append expp $ L.take 11 macOut
  return $ armor32 out

unpickePriv :: PrivStore -> Principal -> String -> DC (Maybe DCPrivs)
unpickePriv ps princ cookie = do
  let bcookie = dearmor32 cookie
  (TOD now _) <- ioTCB $ getClockTime
  let expire = unserializele $ take 5 $ L.unpack bcookie
  if L.length bcookie /= 16 || now > expire
    then return Nothing
    else do
      p <- ioTCB $ picklePrivTCB ps princ expire
      return $ if p == cookie then Just (mintTCB princ) else Nothing

initPS :: IO PrivStore
initPS = do
  let prefix = defaultPrefix
      kf = prefix </> keyfile
  exists <- doesFileExist kf
  k <- if exists
       then L.readFile kf
       else do
         createDirectoryIfMissing True $ takeDirectory kf
         k' <- getRandomBytes
         L.writeFile kf k'
         return k'
  return PS { psPrefix = prefix, psKey = k }


prin :: Principal
prin = Principal "hello"
priv :: DCPrivs
priv = mintTCB $ Principal "hello"

main :: IO ()
main = initPS >> return ()
