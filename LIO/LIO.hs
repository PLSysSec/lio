-- | This is the main module to be included by code using the Labeled
-- IO (LIO) library.  The core of the library is documented in the
-- "LIO.TCB" module.  Note, however, the unprivileged code must not be
-- allowed to import "LIO.TCB"--instead, a module "LIO.Base" exports
-- just the safe symbols from "LIO.TCB".  This module, "LIO.LIO",
-- rexports "LIO.Base" as well as a few other handy modules.  For many
-- modules it should be the only import necessary.
--
-- Certain symbols in the LIO library supersede variants in the
-- standard Haskell libraries.  Thus, depending on the modules
-- imported and functions used, you may wish to import LIO with
-- commands like these:
--
-- > import Prelude hiding (readFile, writeFile, catch)
-- > import Control.Exception hiding (throwIO, catch, onException
-- >                                 , bracket, block, unblock)
-- > import LIO.LIO
--
-- Where possible the LIO variants of the functions should work in
-- both the IO and LIO monads, making it easier to put have both types
-- of code in the same module.
module LIO.LIO (module LIO.Base
               , module LIO.Handle
               , module LIO.LIORef
               ) where

import LIO.Base
import LIO.Handle
import LIO.LIORef
import LIO.MonadLIO
