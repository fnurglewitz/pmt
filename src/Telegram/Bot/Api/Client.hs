{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Telegram.Bot.Api.Client where

import App.Config
import App.Monad
import Control.Arrow (left)
import Control.Concurrent.MVar
import Control.Exception (Handler (Handler), catch, catches, throwIO)
import Control.Monad.Except (liftEither)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, liftIO, runReaderT)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lens.Micro.Platform
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ResponseTimeout), managerResponseTimeout, responseTimeoutDefault, responseTimeoutMicro)
import Network.HTTP.Client.Internal (Response (Response))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq
import Telegram.Bot.Api.Types

class (Monad m) => TelegramClient m where
  getUpdate :: m (Maybe Update)
  sendMessage :: SendMessageRequest -> m ()
  editMessage :: EditMessageRequest -> m ()
  sendPhoto :: SendPhotoRequest -> m ()

instance (MonadIO m, e ~ Text) => TelegramClient (AppM a e m) where
  getUpdate = do
    AppCtx {..} <- ask
    offset <- liftIO $ (+ 1) <$> readMVar lastTelegramUpdateId
    let opts =
          defaults
            & param "offset" .~ [T.pack $ show offset]
            & param "limit" .~ ["1"]
            & param "timeout" .~ ["30"]
            & param "allowed_updates" .~ ["[\"message\", \"callback_query\"]"]
            & manager
              .~ Left
                ( tlsManagerSettings
                    { managerResponseTimeout = responseTimeoutMicro 35000000
                    }
                )
        TelegramConfig tgBaseUrl (Token token) _ = telegramCfg config
    let call = view responseBody <$> getWith opts (T.unpack (T.concat [tgBaseUrl, token, "/getUpdates"]))
    responseMB <- liftIO $
      catch (Just <$> call) $
        \(HttpExceptionRequest req content) -> do
          --liftIO $ print content
          pure Nothing
    case responseMB of
      Nothing -> pure Nothing
      Just responseB -> do
        response <- liftEither $ left T.pack $ eitherDecode responseB
        case response of
          (TelegramResponse False _) -> pure Nothing
          (TelegramResponse True []) -> pure Nothing
          (TelegramResponse True (x : _)) -> do
            liftIO $ swapMVar lastTelegramUpdateId $ updateId x
            pure $ Just x

  sendMessage SendMessageRequest {..} = do
    TelegramConfig tgBaseUrl (Token token) _ <- asks $ telegramCfg . config
    let partChatId = partText "chat_id" mChatId
        partTxt = partText "text" mText
        partDisableNotification = partText "disable_notification" $ (T.pack . show) mDisableNotification
        partReplyTo = maybeToList $ partText "reply_to_message_id" . T.pack . show <$> mReplyToMsgId
        partParseMode = partText "parse_mode" "HTML"
        partKb = maybeToList $ partText "reply_markup" . decodeUtf8 . BL.toStrict . encode <$> mInlineKeyboard
    liftIO $ post (T.unpack (T.concat [tgBaseUrl, token, "/sendMessage"])) $ [partChatId, partTxt, partDisableNotification, partParseMode] ++ partReplyTo ++ partKb
    return ()

  editMessage EditMessageRequest {..} = do
    TelegramConfig tgBaseUrl (Token token) _ <- asks $ telegramCfg . config
    let partChatId = partText "chat_id" eChatId
        partMsgId = partText "message_id" eMessageId
        partTxt = partText "text" eText
        partKb = maybeToList $ partText "reply_markup" . decodeUtf8 . BL.toStrict . encode <$> eInlineKeyboard
    liftIO $ post (T.unpack (T.concat [tgBaseUrl, token, "/editMessageText"])) $ [partChatId, partMsgId, partTxt] ++ partKb
    return ()

  sendPhoto SendPhotoRequest {..} = do
    TelegramConfig tgBaseUrl (Token token) _ <- asks $ telegramCfg . config
    let partChatId = partText "chat_id" pChatId
        -- partCaption = partText "caption" want
        partPhoto = partBS "photo" content & (partFileName ?~ "photo.png") & (partContentType ?~ "image/png")
        partKb = maybeToList $ partText "reply_markup" . decodeUtf8 . BL.toStrict . encode <$> pInlineKeyboard
    -- partKb = partText "reply_markup" $ decodeUtf8 $ BL.toStrict $ encode (InlineKeyboardMarkup [[InlineKeyboardButton want (Just url) Nothing], [InlineKeyboardButton "stop tracking" Nothing (Just (T.append "delete:" tradeId))]])
    liftIO $ post (T.unpack (T.concat [tgBaseUrl, token, "/sendPhoto"])) $ [partChatId, partPhoto] ++ partKb
    return ()
