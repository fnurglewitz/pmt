{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Auth where

import App.Config
import Control.Monad.Except
  ( MonadError (throwError),
  )
import Control.Monad.Reader
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import Data.Time ( getCurrentTime )
import Database.PostgreSQL.Simple
import qualified Database.Types as DB
import Logging.Types
import Telegram.Bot.Api.Client
import Telegram.Bot.Api.Types

checkAuth :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => User -> m DB.Auth
checkAuth User {..} = do
  AppCtx {..} <- ask
  let adminId = tgAdmin . telegramCfg $ config
  if userId == adminId
    then do return $ DB.Auth adminId undefined True 9999 9999 "admin" -- crap
    else do
      mbAuth <- DB.getAuth userId
      case mbAuth of
        Nothing -> do
          now <- liftIO getCurrentTime
          DB.saveAuth $ DB.Auth userId now False 0 0 (fromMaybe "" username) -- TODO
          sendMessage $ SendMessageRequest userId "Unauthorized" True Nothing Nothing
          throwError "Unauthorized"
        Just auth@DB.Auth {..} -> do
          if aEnabled
            then return auth
            else do
              sendMessage $ SendMessageRequest userId "Unauthorized" True Nothing Nothing
              throwError "Unauthorized"
