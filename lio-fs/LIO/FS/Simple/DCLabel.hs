{-# LANGUAGE Safe #-}
{- |

This module exposes some wrapper functions for executing 'LIO' actions
using 'DCLabel's with simple ("LIO.FS.Simple") filesystem support.

-}

module LIO.FS.Simple.DCLabel (
    evalDCWithRoot 
  , tryDCWithRoot 
  ) where

import safe LIO
import safe LIO.DCLabel
import safe LIO.FS.Simple

-- | Like 'evalDC', execute a 'DC' action, but with filesystem
-- support. The filesystme root is supplied, while the root label is
-- 'dcPublic'. See "LIO.FS.Simple" for a description of the simple
-- filesystem API.
evalDCWithRoot :: FilePath -- ^ Filesystem root
               -> DC a     -- ^ LIO action
               -> IO a
evalDCWithRoot root dc = evalLIOWithRoot root (Just dcPublic) dc dcDefaultState

-- | Similar to 'evalDCWithRoot', but catches the end exception. See
-- 'tryDC'.
tryDCWithRoot :: FilePath -- ^ Filesystem root
              -> DC a     -- ^ LIO action
              -> IO (Either SomeException a, LIOState DCLabel)
tryDCWithRoot root dc = tryLIOWithRoot root (Just dcPublic) dc dcDefaultState
