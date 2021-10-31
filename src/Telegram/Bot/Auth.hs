{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Auth where

import App.Config
import qualified App.Database as DB
import App.Logging
import App.Monad
import Control.Exception
import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (..),
    forever,
    runExceptT,
  )
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
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
          sendMessage $ SendMessageRequest (T.pack . show $ userId) "Unauthorized" True Nothing Nothing
          throwError "Unauthorized"
        Just auth@DB.Auth {..} -> do
          if aEnabled
            then return auth
            else do
              sendMessage $ SendMessageRequest (T.pack . show $ userId) "Unauthorized" True Nothing Nothing
              throwError "Unauthorized"
