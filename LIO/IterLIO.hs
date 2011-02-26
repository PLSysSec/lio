{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LIO.IterLIO ( evalIterLIO
                   , liftIterLIO
                   , ioHttpServerLIO
                   , inumHttpServerLIO
                   , enumNonBinHandleLIO 
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

import qualified Data.ListLike as LL
import Data.ByteString.Lazy.Internal (defaultChunkSize)

type L = L.ByteString

--instance (MonadLIO m l s, MonadTrans t, Monad (t m)) => MonadLIO (t m) l s where
--    liftLIO = lift . liftLIO

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
               s -> Iter t (LIO l s) a -> Iter t IO (a, l)
evalIterLIO s0 m0 = do
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

enumNonBinHandleLIO :: (Label l, MonadLIO m l s, ChunkData t, LL.ListLikeIO t e) =>
                    SIO.Handle
                 -> Onum t m a
enumNonBinHandleLIO h = mkInumM $ do
  setCtlHandler (fileCtlLIO h)
  irepeat $ liftLIO (rtioTCB $ SIO.hWaitForInput h (-1) >>
                    LL.hGetNonBlocking h defaultChunkSize) >>= ifeed1

fileCtlLIO :: (ChunkData t, MonadLIO m l s, Label l) => SIO.Handle -> CtlHandler (Iter t m)
fileCtlLIO h = (\(SeekC mode pos) -> liftLIO (rtioTCB $ SIO.hSeek h mode pos))
            `consCtl` (\TellC -> liftLIO (rtioTCB $ SIO.hTell h))
            `consCtl` (\SizeC -> liftLIO (rtioTCB $ SIO.hFileSize h))
            `consCtl` passCtl
