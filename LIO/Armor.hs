-- | These functions support a simple base-32 encoding of binary data,
-- in which 5 bytes of binary data are mapped onto 8 characters from
-- the set {a, ..., k, m, n, p, ..., z, 2, ..., 9} (that is to say all
-- lower-case letters and digits except for l, o, 0, and 9.
--
-- The 'armor32' function encodes binary using this base-32 encoding,
-- while 'dearmor32' reverses the encoding.
--
-- Binary data is assumed to come from the "Data.ByteString.Lazy" type.
module LIO.Armor (armor32, dearmor32) where

import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Bits
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char
import Data.Word

a2b :: UArray Word8 Char
a2b = listArray (0, 31) $ do c <- ['a'..'z'] ++ ['0' .. '9']
                             guard $ not $ elem c "lo01"
                             return c

armor32 :: L.ByteString -> String
armor32 s = doit 0 $ L.unpack s
    where
      doit _ [] = []
      doit skip s@(c1:s1) =
          let hi = shift c1 (skip - 3) .&. 0x1f
              lo = if skip <= 3 || s1 == []
                   then 0
                   else shift (head s1) (skip - 11)
              c = a2b ! (hi .|. lo)
          in if skip >= 3
             then c : doit (skip - 3) s1
             else c : doit (skip + 5) s

inval = -1 :: Word8
b2a :: UArray Char Word8
b2a = accumArray (\a b -> b) inval (chr 0, chr 255)
      [(y, x) | (x, y) <- assocs a2b]

dearmor32 :: String -> L.ByteString
dearmor32 s = doit 0 0 s
    where
      doit _ _ [] = L.empty
      doit carryVal carrySize (c1:s) =
          let v = b2a ! c1
          in if v == inval
             then L.empty
             else let needbits = 8 - carrySize
                      nextCarrySize = 5 - needbits
                      b = carryVal .|. (shift (b2a ! c1) (negate nextCarrySize))
                      nextCarry = shift v (8 - nextCarrySize)
                  in if nextCarrySize < 0
                     then doit b (nextCarrySize + 8) s
                     else L.cons b $ doit nextCarry nextCarrySize s


{-
mask n = complement $ shift (fromInteger $ -1) n
pack s = LC.pack s
-}

