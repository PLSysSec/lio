{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Unsafe #-}
#endif
-- | This module simply export a type for 'Labeled' 'Handle's
module LIO.Handle.TCB ( LHandle(..), labelOfHandle) where

import qualified System.IO as IO
import LIO (Label)

--
-- Labeled Handle
--

-- | A labeled handle.
data LHandle l = LHandleTCB l IO.Handle


-- | Get the label of a labeled handle.
labelOfHandle :: Label l => LHandle l -> l
labelOfHandle (LHandleTCB l _) = l
