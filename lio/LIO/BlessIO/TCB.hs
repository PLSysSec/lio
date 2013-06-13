{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- | This module provides routines for safely exposing IO functions in
-- the 'LIO' monad.  At a high level, certain IO objects such as
-- handles can be 'Blessed' as safe to access at a particular label,
-- while certain operations can then be blessed (via 'blessTCB') to
-- operate on such blessed objects.
--
-- For example, trusted code might define the following:
--
-- > import qualified System.IO as IO
-- > 
-- > type Handle = Blessed DCLabel IO.Handle
-- > 
-- > hPutStrLn :: Blessed DCLabel IO.Handle -> String -> LIO DCLabel ()
-- > hPutStrLn h = blessTCB IO.hPutStrLn h
-- > 
-- > hGetLine :: Blessed DCLabel IO.Handle -> LIO DCLabel String
-- > hGetLine h = blessTCB IO.hGetLine h
--
-- Then application-specific trusted code can wrap a specific label
-- around each socked using the 'BlessedTCB' constructor.
module LIO.BlessIO.TCB (Blessed(..), blessTCB, blessPTCB) where

import safe LIO.Core
import safe LIO.Label
import safe LIO.Privs
import LIO.TCB

-- | A @Blessed label object@ is a wrapper around an IO abstraction of
-- type @object@ (such as a file handle or socket) on which it is safe
-- to do @IO@ operations in the 'LIO' monad when the caller can read
-- and write a particular label.  It is the job of the trusted code
-- constructing such a @Blessed@ object to ensure both that the same
-- IO object is only ever blessed with one label, and that the
-- abstraction combined with its blessed IO operations (see
-- 'blessTCB') cannot be used to communicate with code running at
-- different labels.
data Blessed label object = BlessedTCB !label !object

instance LabelOf Blessed where
  labelOf (BlessedTCB l _) = l

guardIOPTCB :: (Label l, PrivDesc l p) => Priv p -> l -> IO a -> LIO l a
guardIOPTCB p l io = guardWriteP p l >> rethrowIoTCB io

class (Label l) => BlessIO l io lio | l io -> lio where
  -- | A version of 'blessTCB' that takes privileges.
  blessPTCB :: (PrivDesc l p) => Priv p -> (a -> io) -> (Blessed l a -> lio)
instance (Label l) => BlessIO l (IO r) (LIO l r) where
  blessPTCB p io (BlessedTCB l a) = guardIOPTCB p l (io a)

#define BLESSIO(types, vals) \
instance (Label l) => BlessIO l (types -> IO r) (types -> LIO l r) where \
  blessPTCB p io (BlessedTCB l a) vals = guardIOPTCB p l (io a vals)

BLESSIO (b, b)
BLESSIO (b -> c, b c)
BLESSIO (b -> c -> d, b c d)
BLESSIO (b -> c -> d -> e, b c d e)
BLESSIO (b -> c -> d -> e -> f, b c d e f)
BLESSIO (b -> c -> d -> e -> f -> g, b c d e f g)
BLESSIO (b -> c -> d -> e -> f -> g -> h, b c d e f g h)
BLESSIO (b -> c -> d -> e -> f -> g -> h -> i, b c d e f g h i)
BLESSIO (b -> c -> d -> e -> f -> g -> h -> i -> j, b c d e f g h i j)

-- | This function can be used to turn an 'IO' function into an 'LIO'
-- one.  The 'LIO' version expects a 'Blessed' argument, and before
-- performing any IO uses 'guardWrite' to check that the current label
-- can write the label in the 'Blessed' object.
--
-- Note that @io@ and @lio@ are function types (of up to nine
-- arguments), which must be the same in all types except the monad.
-- For example, if @io@ is @Int -> String -> IO ()@, then @lio@ must
-- be @Int -> String -> LIO l ()@.
blessTCB :: BlessIO l io lio => (a -> io) -> Blessed l a -> lio
blessTCB = blessPTCB noPrivs
