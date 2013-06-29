{-# LANGUAGE Safe #-}
{- |

This is the main module to be included by code using the Labeled IO
(LIO) library. This module exports the core library (documented in
"LIO.Core"), with support for labels and privileges (documented in
"LIO.Label") and labeled values (documented in "LIO.Labeled").

Certain symbols in the LIO library, particularly those in
"LIO.Exception", use the same names as their 'IO' equivalents in the
system libraries.  Hence main modules consisting mostly of 'IO' code
that simply need to run 'LIO' code should import "LIO.Run" (or
"LIO.DCLabel") to avoid polluting their namespaces.

Most code will need to use a particular label format, which needs to
be imported separately.  Hence, a typical set of imports for an
untrusted LIO module is:

@
 import "LIO"
 import "LIO.DCLabel"
@

-}

module LIO ( 
    module LIO.Core
  , module LIO.Delegate
  , module LIO.Exception
  , module LIO.Label
  , module LIO.Labeled
  ) where

import safe LIO.Core
import safe LIO.Delegate
import safe LIO.Exception
import safe LIO.Label hiding (isPriv)
import safe LIO.Labeled
