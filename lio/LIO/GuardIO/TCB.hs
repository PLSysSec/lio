{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- | This module provides routines for safely exposing IO functions in
-- the 'LIO' monad.  At a high level, certain IO objects such as
-- handles can be associated with a label via 'LObj', while certain
-- operations can then be blessed (via 'blessTCB') to operate on such
-- 'LObj' objects.
--
-- For example, trusted code might define the following:
--
-- > import qualified System.IO as IO
-- > 
-- > type Handle = LObj DCLabel IO.Handle
-- > 
-- > hPutStrLn :: LObj DCLabel IO.Handle -> String -> LIO DCLabel ()
-- > hPutStrLn h = blessTCB IO.hPutStrLn noPrivs h
-- > 
-- > hGetLine :: LObj DCLabel IO.Handle -> LIO DCLabel String
-- > hGetLine h = blessTCB IO.hGetLine noPrivs h
--
-- Then application-specific trusted code can wrap a specific label
-- around each 'Handle' using the 'LObjTCB' constructor.
module LIO.GuardIO.TCB (LObj(..), blessTCB, GuardIO(..)) where

import safe LIO.Core
import safe LIO.Label
import safe LIO.Privs
import LIO.TCB

-- | A "@LObj label object@" is a wrapper around an IO abstraction of
-- type @object@ (such as a file handle or socket) on which it is safe
-- to do @IO@ operations in the 'LIO' monad when the caller can read
-- and write a particular label.  It is the job of the trusted code
-- constructing such a @LObj@ object to ensure both that the same IO
-- object is only ever blessed with one label, and that the
-- abstraction combined with its blessed IO operations (see
-- 'blessTCB') cannot be used to communicate with code running at
-- different labels.
data LObj label object = LObjTCB !label !object

instance LabelOf LObj where
  labelOf (LObjTCB l _) = l

#if 0
genTypesVals :: Int -> IO ()
genTypesVals n0 =
  putStrLn $ "#define TypesVals(macro) \\\n" ++ concatMap doit [1..n0]
  where doit n = "  macro(" ++ intercalate " -> " (as n) ++ ", \\\n" ++
                 "        " ++ intercalate " " (as n) ++ ")" ++
                 (if n < n0 then "; \\\n" else "\n")
        as n = map (\i -> "a" ++ show i) [1..n]
#endif

#define TypesVals(macro) \
  macro(a1, \
        a1); \
  macro(a1 -> a2, \
        a1 a2); \
  macro(a1 -> a2 -> a3, \
        a1 a2 a3); \
  macro(a1 -> a2 -> a3 -> a4, \
        a1 a2 a3 a4); \
  macro(a1 -> a2 -> a3 -> a4 -> a5, \
        a1 a2 a3 a4 a5); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6, \
        a1 a2 a3 a4 a5 a6); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7, \
        a1 a2 a3 a4 a5 a6 a7); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8, \
        a1 a2 a3 a4 a5 a6 a7 a8); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9, \
        a1 a2 a3 a4 a5 a6 a7 a8 a9); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10, \
        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

class GuardIO l r io lio | l io -> r lio where
  -- | Wraps a lifter function around the result of a function
  -- returning an 'IO' action to turn it into a function returning an
  -- 'LIO' action.  Internally 'blessTCB' uses this method, supplying
  -- a lifter that check the current label (with 'guardWriteP') and
  -- lifts the 'IO' action (with 'rethrowIoTCB').
  guardIO :: (IO r -> LIO l r) -> io -> lio
instance GuardIO l r (IO r) (LIO l r) where
  guardIO lifter io = lifter io
#define GUARDIO(types, vals) \
instance GuardIO l r (types -> IO r) (types -> LIO l r) where \
  { guardIO lifter io vals = lifter $ io vals }
TypesVals (GUARDIO)

-- | This function can be used to turn an 'IO' function into an 'LIO'
-- one.  The 'LIO' version expects a 'LObj' argument, and before
-- performing any IO uses 'guardWrite' to check that the current label
-- can write the label in the 'LObj' object.
--
-- Note that @io@ and @lio@ are function types (of up to nine
-- arguments), which must be the same in all types except the monad.
-- For example, if @io@ is @Int -> String -> IO ()@, then @lio@ must
-- be @Int -> String -> LIO l ()@.
blessTCB :: (GuardIO l r io lio, PrivDesc l p) =>
            (a -> io) -> Priv p -> (LObj l a) -> lio
blessTCB io p (LObjTCB l a) = guardIO lifter (io a)
  where lifter r = guardWriteP p l >> ioTCB r
