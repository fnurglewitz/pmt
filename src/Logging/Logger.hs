module Logging.Logger where

import App.Config (Config (appName, appVersion, env))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Logging.Types
  ( LogLevel
  , LogMessage
      ( LogMessage
      , lappName
      , lenvironment
      , level
      , lmessage
      , ltimestamp
      , lversion
      )
  )
import System.Log.FastLogger
  ( LoggerSet
  , ToLogStr (..)
  , defaultBufSize
  , flushLogStr
  , newStdoutLoggerSet
  , pushLogStrLn
  )

getLoggerSet :: IO LoggerSet
getLoggerSet = newStdoutLoggerSet defaultBufSize

logGeneric :: LoggerSet -> Text -> Config -> Text -> IO ()
logGeneric logger lvl cfg msg = do
  tstamp <- getCurrentTime
  let lgmsg =
        LogMessage
          { lmessage = msg
          , ltimestamp = tstamp
          , level = lvl
          , lappName = appName cfg
          , lversion = appVersion cfg
          , lenvironment = env cfg
          }
  pushLogStrLn logger (toLogStr lgmsg) >> flushLogStr logger

logAction :: LoggerSet -> Config -> Text -> LogLevel -> Text -> IO ()
logAction logger cfg envLvl lvl =
  if read (T.unpack envLvl) >= lvl
    then logGeneric logger (T.pack . show $ lvl) cfg
    else \_ -> return ()
