{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Trustworthy #-}
#else
#warning "This module is not using SafeHaskell"
#endif

{-| This module provides bindings for the @DCLabel@ module, with some 
renaming to resolve name clashes. The delegation of privilege and 
other trusted code is not exported by this module and code wishing to
use this should import @DCLabel.TCB@.
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LIO.DCLabel ( -- * DCLabel export
  		     module DCLabel.Safe
                   , DCCatSet
                     -- * Renamed privileges
                   , DCPriv, DCPrivTCB
                     -- * Useful aliases for the LIO Monad
                   , DCLabeled, DC, evalDC
                   )where

import LIO.TCB

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
import safe Data.Typeable
#else
import Data.Typeable
#endif

import DCLabel.Safe hiding ( Label
                           , Priv
                           , bottom
                           , top
                           , join
                           , meet)
import qualified DCLabel.Core as DCL


deriving instance Typeable DCL.Disj
deriving instance Typeable DCL.Conj
deriving instance Typeable DCL.Label
deriving instance Typeable DCL.DCLabel


instance Label DCLabel where
	lbot = DCL.bottom
	ltop = DCL.top
	lub  = DCL.join
	glb  = DCL.meet
	leq  = DCL.canflowto

instance PrivTCB DCL.TCBPriv

instance MintTCB DCL.TCBPriv DCL.Priv where
	mintTCB = DCL.createPrivTCB

instance MintTCB DCL.TCBPriv DCL.Principal where
	mintTCB p = DCL.createPrivTCB (newPriv p)

instance Priv DCLabel DCL.TCBPriv where
  	leqp = DCL.canflowto_p
        {-
        The implementation of lostar deserves an explanation. Firstly note
        that the properties, for @r = lostar p l g@ that must be satisfied
        are [the suffix \'s\' (\'i\')is used for seecrecy (resp. integrity):
        1.) @leq g r    : (rs => gs)      and  (gi => ri)@
        2.) @leqp p l r : (rs /\ p => ls) and  (li /\ p => ri)@
        Finding the integrity component of @r@ is trivial: it's
        simply the least upper bound of @gi@ and @li /\ p@.
        Finding the secrecy component is a bit trickier. To do so, we first
        find all the categories of @ls@ that are not implied by @p@ (this
        gives us @rs'@), such that @rs' /\ p => ls@. Then, we need to find
        the remaining categories in @gs@ that are not implied by @rs'@ (this
        gives us @rs''@). Directly, @rs = rs' /\ rs''@.
        -}
        lostar p l g = 
          let (ls, li) = (DCL.toLNF . secrecy $ l, DCL.toLNF . integrity $ l)
              (gs, gi) = (DCL.toLNF . secrecy $ g, DCL.toLNF . integrity $ g)
              lp       = DCL.toLNF . DCL.priv $ p
              rs'      = c2l [c | c <- getCats ls
                                , not (lp `DCL.implies` (c2l [c]))]
              rs''     = c2l [c | c <- getCats gs
                                , not (rs' `DCL.implies` (c2l [c]))]
              rs       = rs' `DCL.and_label` rs''
              ri       = (li `DCL.and_label` lp) `DCL.or_label` gi
         in DCL.toLNF $ simpleNewLabel p (newDC rs ri)
              where getCats = DCL.conj . DCL.label
                    c2l = DCL.MkLabel . DCL.MkConj
                    simpleNewLabel pr lr | pr == DCL.rootPrivTCB = g   
                                         | pr == DCL.noPriv      = l `lub` g
                                         | otherwise             = lr

--
-- Renaming
--

-- | A @DCLabel@ category set.
type DCCatSet = DCL.Label
-- | A @DCLabel@ (untrusted) privilege.
type DCPriv = DCL.Priv
-- | A @DCLabel@ privilege.
type DCPrivTCB = DCL.TCBPriv


--
-- LIO aliases
--

instance LabelState DCLabel () where

-- | The type for 'Labeled' values uinsg 'DCLabel' as the label.
type DCLabeled a = Labeled DCLabel a

-- | The monad for LIO computations using 'DCLabel' as the label.
type DC = LIO DCLabel ()

-- | Runs a computation in the LIO Monad, returning both the
-- computation's result and the label of the result.
evalDC :: DC a -> IO (a, DCLabel)
evalDC m = evalLIO m ()
