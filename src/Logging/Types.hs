{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Logging.Types where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    encode,
    object,
  )
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Log.FastLogger ( ToLogStr(..) )

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

class (Monad m) => HasLogger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()
  logDebug :: Text -> m ()
