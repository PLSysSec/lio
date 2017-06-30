{-# LANGUAGE Unsafe #-}

{- | This module exports a set of loggers in the DC monad. We mark this
   module as @Unsafe@ since it allows for writing to the console and
   files without checking labels.
-}

module LIO.HTTP.Server.Frankie.Loggers (
  -- * Simple, default loggers
  stdOutLogger, stdErrLogger, 
  -- * Standard out and error loggers
  colorStdOutLogger, colorStdErrLogger, 
  simpleStdOutLogger, simpleStdErrLogger, 
  -- * File logger
  openFileLogger
) where

import LIO.TCB (ioTCB)
import LIO.DCLabel
import LIO.HTTP.Server.Frankie
import Data.Time.LocalTime (getZonedTime)
import System.IO
import Control.Monad.Trans.Class (lift)
import System.Console.ANSI

-- | Logger that prints to standard out, using color for TTYs.
stdOutLogger :: Logger DC
stdOutLogger = Logger $ \level str -> do
  isTTY <- ioTCB $ hIsTerminalDevice stdout
  hLog isTTY stdout level str

-- | Logger that prints to standard err, using color for TTYs.
stdErrLogger :: Logger DC
stdErrLogger = Logger $ \level str -> do
  isTTY <- ioTCB $ hIsTerminalDevice stderr
  hLog isTTY stderr level str

-- | Logger that prints to standard out
colorStdOutLogger :: Logger DC
colorStdOutLogger = Logger $ hLog True stdout

-- | Logger that prints to standard out
colorStdErrLogger :: Logger DC
colorStdErrLogger = Logger $ hLog True stderr

-- | Logger that prints to standard out
simpleStdOutLogger :: Logger DC
simpleStdOutLogger = Logger $ hLog False stdout

-- | Logger that prints to standard out
simpleStdErrLogger :: Logger DC
simpleStdErrLogger = Logger $ hLog False stderr

-- | Logger that prints string to handle
hLog :: Bool -> Handle -> LogLevel -> String -> DC ()
hLog useColor h level str = ioTCB $ do
  time <- getZonedTime 
  if useColor
    then putColorStrLn level h $ show time ++ " " ++ show level ++ ": " ++ str
    else hPutStrLn h $ show time ++ " " ++ show level ++ ": " ++ str

-- | Print to handle, coloring the line according to the level.
putColorStrLn :: LogLevel -> Handle -> String -> IO ()
putColorStrLn level h str = do
  canColor <- hSupportsANSI h
  if canColor
    then do hSetSGR h [SetColor Background Dull Black,
                      SetColor Foreground Vivid (levelToColor level)]
            hPutStrLn h str
            hSetSGR h [Reset]
    else hPutStrLn h str

-- | Convert level to color
levelToColor :: LogLevel -> Color
levelToColor EMERGENCY = Red
levelToColor ALERT     = Red
levelToColor CRITICAL  = Red
levelToColor ERROR     = Red
levelToColor WARNING   = Yellow
levelToColor NOTICE    = Blue
levelToColor INFO      = White
levelToColor DEBUG     = Magenta



-- | Create a new logger that writes to the given path. Note that there
-- is no clean way to clean up the file descriptor once the file is open.
-- In general is is okay because we expect the logger to remain live for
-- the lifetime of the application.
openFileLogger :: FrankieConfigMonad k => FilePath -> k s DC (Logger DC)
openFileLogger path = liftFrankie $ FrankieConfig $ lift $ do
  handle <- openFile path AppendMode
  hSetBuffering handle LineBuffering
  return $ Logger $ hLog False handle
