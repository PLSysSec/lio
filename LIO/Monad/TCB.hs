{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveFunctor,
             GeneralizedNewtypeDeriving #-}
{- | 

This module exports the definition of the 'LIO' monad and relevant
trusted state access/modifying functions. See "LIO.Monad" for a safe export of this API.

-}

module LIO.Monad.TCB (
  -- * LIO state
    LIOState(..), CallTrace(..)
  -- * LIO Monad
  , LIO(..)
  , getLIOStateTCB, putLIOStateTCB, updateLIOStateTCB 
  -- * IO helpers
  , ioTCB
  ) where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Loc
       
import LIO.Label

--
-- LIO Monad
--

-- | Call trace for 'LIO' monad
newtype CallTrace = CallTrace [String]
  deriving Eq

instance Show CallTrace where
  show (CallTrace cs) = unlines . reverse $ cs

instance Read CallTrace where
  readsPrec _ str = [(CallTrace . reverse $ lines str, "")]

-- | Internal state of an 'LIO' computation.
data LIOState l = LIOState {
    lioLabel     :: !l         -- ^ Current label
  , lioClearance :: !l         -- ^ Current clearance
  , lioCallTrace :: CallTrace  -- ^ Current call trace
  } deriving (Eq, Show, Read)

-- | The @LIO@ monad is a state monad, with 'IO' as the underlying monad,
-- that carries along a /current label/ ('lioLabel') and /current
-- clearance/ ('lioClearance'). The current label imposes restrictions on
-- what the current computation may read and write (e.g., no writes to
-- public channels after reading sensitive data).  Since the current
-- label can be raised to be permissive in what a computation observes,
-- we need a way to prevent certain computations from reading overly
-- sensitive data. This is the role of the current clearance: it imposes
-- an upper bound on the current label.
--
-- Finally, to improve debugging and present meaningful error messages
-- the underlying state keeps a call trace (using @monadloc@).
newtype LIO l a = LIOTCB { unLIOTCB :: StateT (LIOState l) IO a }
  deriving (Functor, Applicative, Monad)

instance Label l => MonadLoc (LIO l) where
  withLoc loc act =  do
    s <- getLIOStateTCB 
    let l = show . lioLabel $ s
        c = show . lioClearance $ s
        (CallTrace t) = lioCallTrace s
        s' = s { lioCallTrace = CallTrace $
                  ( loc ++ ";\n\t Label = "     ++ l
                        ++ "; Clearance = " ++ c) : t }
    putLIOStateTCB s'
    act


-- | Get internal state. This function is not actually unsafe, but
-- to avoid future security bugs we leave all direct access to the
-- internal state to trusted code.
getLIOStateTCB :: Label l => LIO l (LIOState l)
getLIOStateTCB = LIOTCB . StateT $! \s -> return (s, s)

-- | Set internal state.
putLIOStateTCB :: Label l => LIOState l -> LIO l ()
putLIOStateTCB s = LIOTCB . StateT $! \_ -> return ((), s)

-- | Update the internal state given some function.
updateLIOStateTCB :: Label l => (LIOState l -> LIOState l) -> LIO l ()
updateLIOStateTCB f = do
  s <- getLIOStateTCB
  putLIOStateTCB $! f s

-- | Lifts an 'IO' computation into the 'LIO' monad.  Note that exceptions thrown
-- within the 'IO' computation cannot directly be caught within the 'LIO'
-- computation.  Thus, you will generally want to use 'rtioTCB' exported by
-- "LIO.Exception.TCB" instead of 'ioTCB'.
ioTCB :: Label l => IO a -> LIO l a
ioTCB a = LIOTCB . StateT $! \s -> do
  r <- a
  return (r, s)
