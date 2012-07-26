{-# LANGUAGE Safe #-}
{- |

This is the main module to be included by code using the Labeled IO
(LIO) library. This module exports the core library (documented in
"LIO.Core"), with support for labeled values (documented in
"LIO.Labeled"), privileges (documented in "LIO.Privs"), and gates
(documented in "LIO.Gate").

Certain symbols in the LIO library supersede variants in the
standard Haskell libraries.  Thus, depending on the modules
imported and functions used, you may wish to import LIO with
commands like these:

@
 import Control.Exception hiding ( 'onException'
                                 , 'finally'
                                 , 'bracket')
 import "LIO"
 -- Import your favorite label format:
 import "LIO.DCLabel"
@

WARNING:  For security, untrusted code must always be compiled with
the @-XSafe@ and @-fpackage-trust@ /SafeHaskell/ flags. See
<http://hackage.haskell.org/trac/ghc/wiki/SafeHaskell> for more
details on the guarantees provided by SafeHaskell.

-}

module LIO ( 
    module LIO.Label
  , module LIO.Core
  , module LIO.MonadLIO
  , module LIO.Labeled
  , module LIO.Privs
  , module LIO.Gate
  ) where

import           LIO.Label
import           LIO.Core
import           LIO.MonadLIO
import           LIO.Labeled
import           LIO.Privs
import           LIO.Gate
