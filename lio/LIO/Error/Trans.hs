{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{- | 'MonadLIO' generalizations for "LIO.Error". -}
module LIO.Error.Trans where

import safe LIO.Label
import safe LIO.Monad

import safe qualified LIO.Error as E

-- | See 'LIO.Error.labelError'.
labelError :: (MonadLIO l m, Label l)
    => String -- ^ Function that failed.
    -> [l]    -- ^ Labels involved in error.
    -> m a
labelError fl = liftLIO . E.labelError fl

-- | See 'LIO.Error.labelErrorP'.
labelErrorP :: (MonadLIO l m, Label l, PrivDesc l p)
    => String  -- ^ Function that failed.
    -> Priv p  -- ^ Privileges involved.
    -> [l]     -- ^ Labels involved.
    -> m a
labelErrorP fl p = liftLIO . E.labelErrorP fl p
