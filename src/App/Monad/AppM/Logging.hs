{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Monad.AppM.Logging where

import App.Config
  ( AppCtx (AppCtx),
    Config (Config, appName, appVersion, env, logLevel),
  )
import App.Monad.AppM ( AppM )
import Control.Monad.Reader (MonadIO, ask, liftIO)
import Logging.Types ( LogLevel(DEBUG, INFO, ERROR), HasLogger(..) )
import Logging.Logger ( logAction )
import System.Log.FastLogger
  ( LoggerSet,
    defaultBufSize,
    newStdoutLoggerSet,
  )

getLoggerSet :: IO LoggerSet
getLoggerSet = newStdoutLoggerSet defaultBufSize

instance (MonadIO m) => HasLogger (AppM a e m) where
  logInfo msg = do
    AppCtx cfg@Config {..} logger _ _ _ <- ask
    liftIO $ logAction logger cfg logLevel INFO msg
  logError msg = do
    AppCtx cfg@Config {..} logger _ _ _ <- ask
    liftIO $ liftIO $ logAction logger cfg logLevel ERROR msg
  logDebug msg = do
    AppCtx cfg@Config {..} logger _ _ _ <- ask
    liftIO $ liftIO $ logAction logger cfg logLevel DEBUG msg