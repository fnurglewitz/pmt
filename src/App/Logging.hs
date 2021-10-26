{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Logging (HasLogger (..), getLoggerSet, logGeneric) where

import App.Config
  ( AppCtx (AppCtx),
    Config (Config, appName, appVersion, env, logLevel),
  )
import App.Monad (AppM)
import Control.Monad.Reader (MonadIO, ask, liftIO)
import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    encode,
    object,
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr (..),
    defaultBufSize,
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStrLn,
  )

getLoggerSet :: IO LoggerSet
getLoggerSet = newStdoutLoggerSet defaultBufSize

data LogLevel = ERROR | INFO | DEBUG deriving (Eq, Ord, Show, Read)

data LogMessage = LogMessage
  { lmessage :: Text,
    ltimestamp :: UTCTime,
    level :: Text,
    lappName :: Text,
    lversion :: Text,
    lenvironment :: Text
  }
  deriving (Generic)

instance ToJSON LogMessage where
  toJSON LogMessage {..} =
    object
      [ "message" .= lmessage,
        "timestamp" .= ltimestamp,
        "level" .= level,
        "appName" .= lappName,
        "version" .= lversion,
        "env" .= lenvironment
      ]

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

logGeneric :: LoggerSet -> Text -> Config -> Text -> IO ()
logGeneric logger lvl cfg msg = do
  tstamp <- getCurrentTime
  let lgmsg =
        LogMessage
          { lmessage = msg,
            ltimestamp = tstamp,
            level = lvl,
            lappName = appName cfg,
            lversion = appVersion cfg,
            lenvironment = env cfg
          }
  pushLogStrLn logger (toLogStr lgmsg) >> flushLogStr logger

class (Monad m) => HasLogger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()
  logDebug :: Text -> m ()

logAction :: LoggerSet -> Config -> Text -> LogLevel -> Text -> IO ()
logAction logger cfg envLvl lvl = if read (T.unpack envLvl) >= lvl then logGeneric logger (T.pack . show $ lvl) cfg else \_ -> return ()

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
