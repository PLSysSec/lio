{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LIO.IterLIO ( evalIterLIO
                   , liftIterLIO
                   , ioHttpServerLIO
                   , inumHttpServerLIO
                   -- * Trusted code
                   , mkIterLIOTCB
                   ) where

import Prelude
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.IterIO
import Data.IterIO.Http
import Data.Monoid
import qualified System.IO as SIO
import Data.Time (getCurrentTime)
import System.IO.Error (isEOFError)

import LIO.LIO hiding (liftIO)
import LIO.TCB
import qualified LIO.MonadCatch as LIO

import qualified Data.ByteString.Lazy as L
import Control.Applicative


type L = L.ByteString

instance (ChunkData t, MonadLIO m l s, Label l) =>
    MonadLIO (Iter t m) l s where
    liftLIO m = do
      result <- lift $ liftLIO $ liftM Right m `LIO.catch` (return . Left)
      case result of
        Right ok -> return ok
        Left err -> flip IterFail mempty $
               case E.fromException err of
                 Just ioerr | isEOFError ioerr -> E.toException $ IterEOF ioerr
                 _                             -> err

runIterLIO :: (ChunkData t, Label l) =>
              Iter t (LIO l s) a -> LIOstate l s -> Iter t IO (a, LIOstate l s)
runIterLIO m0 s0 = adaptIter (\a -> (a, s0)) adapt m0
    where adapt m1 = do
            (m2, s) <- liftIO $ runLIO m1 s0
            runIterLIO m2 s

evalIterLIO :: (ChunkData t, Label l) =>
              Iter t (LIO l s) a -> s -> Iter t IO (a, l)
evalIterLIO m0 s0 = do
  liftM (\(a,s) -> (a, lioL s)) $ runIterLIO m0 (newstate s0)
 where newstate s = LIOstate { labelState = s , lioL = lpure , lioC = lclear }

mkIterLIOTCB :: (ChunkData t, Label l) =>
                Iter t IO a -> Iter t (LIO l s) a
mkIterLIOTCB = adaptIterM rtioTCB

liftIterLIO :: (ChunkData t, MonadLIO m l s) => Iter t (LIO l s) a -> Iter t m a
liftIterLIO = adaptIterM liftLIO



instance Show (Lref l a) where
  show _ = error "**SHOULD NEVER USE show on Lrefs**"

ioHttpServerLIO :: (Label l, MonadLIO m l s) => HttpRequestHandler m -> HttpServerConf m
ioHttpServerLIO handler = HttpServerConf {
                         srvLogger = liftLIO . rtioTCB . SIO.hPutStrLn SIO.stderr
                       , srvDate = liftLIO . rtioTCB $ Just `liftM` getCurrentTime
                       , srvHandler = handler
                       }
-- | An 'Inum' that behaves like an HTTP server.
inumHttpServerLIO :: (Label l, MonadLIO m l s) =>
                  HttpServerConf m  -- ^ Server configuration
               -> Inum L.ByteString L.ByteString m ()
inumHttpServerLIO server = mkInumM loop
    where
      loop = do
        eof <- atEOFI
        unless eof doreq
      doreq = do
        req <- httpReqI
        let handler = srvHandler server req
        resp <- liftIterM $ inumHttpBody req .|
                (catchI handler errHandler <* nullI)
        now <- liftIterM $ srvDate server
        catchOrI (irun $ enumHttpResp resp now) fatal (const $ loop)
      errHandler e@(E.SomeException _) _ = do
        srvLogger server $ "Response error: " ++ show e
        return $ resp500 $ show e
      fatal e@(E.SomeException _) = do
        liftIterM $ srvLogger server $ "Reply error: " ++ show e
        return ()
