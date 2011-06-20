{- | This is the main module to be included by code using the Labeled
   IO (LIO) library.  The core of the library is documented in the
   "LIO.TCB" module.  Note, however, that unprivileged code must not
   be allowed to import "LIO.TCB"--instead, a module "LIO.Base"
   exports just the safe symbols from "LIO.TCB".  This module,
   "LIO.LIO", re-exports "LIO.Base" as well as a few other handy
   modules.  For many modules it should be the only import necessary.

   Certain symbols in the LIO library supersede variants in the
   standard Haskell libraries.  Thus, depending on the modules
   imported and functions used, you may wish to import LIO with
   commands like these:

   @
    import Prelude hiding ('readFile', 'writeFile', 'catch')
    import Control.Exception hiding ('throwIO', 'catch', 'handle', 'onException'
                                    , 'bracket', 'block', 'unblock')
    import LIO.LIO
   @

   The LIO variants of the system functions hidden in the above import
   commands are designed to work in both the IO and LIO monads, making
   it easier to have both types of code in the same module.


        Warning:  For security, at a minimum untrusted code must not be
        allowed to do any of the following:

          * Import "LIO.TCB",

          * Use any symbols with names ending @...TCB@,

          * Use the @foreign@ keyword,

          * Use functions such as 'unsafePerformIO', 'unsafeInterleaveIO',
            'inlinePerformIO',

          * Use language extensions such as Generalized Newtype
            Deriving and Stand-alone Deriving to extend LIO types
            (such as by deriving an instance of @'Show'@ for 'Lref',
            or deriving an instance of the @'MonadTrans'@ class for
            'LIO', which would allow untrusted code to bypass all
            security with 'lift'),

          * Manually define @typeOf@ methods (as this would cause the
            supposedly safe 'cast' method to make usafe casts); automatically
            deriving @Typeable@ should be safe.

          * Define new 'Ix' instances (which could produce out of bounds
            array references).

        In general, pragmas and imports should be highly scrutinized.  For
        example, most of the "Foreign" class of modules are probably
        dangerous. With GHC 7.2, we will use the SafeHaskell extension 
        to enforce these.
-}
module LIO.LIO (module LIO.Base
               , module LIO.Handle
               , module LIO.LIORef
               , module LIO.MonadLIO
               ) where

-- import Prelude hiding (readFile, writeFile, catch)
import LIO.Base
import LIO.Handle
import LIO.LIORef
import LIO.MonadLIO
