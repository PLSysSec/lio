{-# LANGUAGE Trustworthy #-}
{- | 

This module exports 'throwLIO', which can be used to throw exceptions
labeled with the current label. This function is implemented in this
module solely to avoid circular imports: "LIO.Exception"\'s 'catchLIO'
relies on "LIO.Guards" which, in turn, relies on the use of 'throwLIO.

-}

module LIO.Exception.Throw ( throwLIO ) where
import           LIO.Label
import           LIO.Monad
import           LIO.Exception.TCB 
import           Control.Exception

-- | Throw an exception. The label on the exception is the current
-- label.
throwLIO :: (Exception e, Label l) => e -> LIO l a
throwLIO e = do
  l <- getLabel
  unlabeledThrowTCB $! LabeledExceptionTCB l (toException e)
