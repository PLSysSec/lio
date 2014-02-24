{-# LANGUAGE Safe #-}
{- |

This module exposes a function for initializing a labeled filestore
with the default label 'dcPublic'.

-}

module LIO.FS.Simple.DCLabel (
    initializeDCFS
  , withDCFS
  ) where

import safe Control.Monad (void)
import safe LIO.DCLabel
import safe LIO.FS.Simple (initializeLIOFS, withLIOFS)

-- | Initialize root filesystem at supplied path with public label.
initializeDCFS :: FilePath -> IO ()
initializeDCFS path = void $ initializeLIOFS path (Just dcPublic)

-- | Top-level IO wrapper for using filesystem.
withDCFS :: FilePath -> IO a -> IO a
withDCFS path = withLIOFS path (Just dcPublic)
