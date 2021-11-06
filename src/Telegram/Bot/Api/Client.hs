{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Telegram.Bot.Api.Client where

import App.Config
  ( AppCtx
      ( AppCtx
      , config
      , db
      , lastTelegramUpdateId
      , logger
      , renderingFont
      )
  , Config (telegramCfg)
  , TelegramConfig (TelegramConfig)
  )
import App.Monad.AppM (AppM)
import Control.Arrow (left)
import Control.Concurrent.MVar (readMVar, swapMVar)
import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.Except (liftEither)
import Control.Monad.Reader (MonadIO, ask, asks, liftIO)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro.Platform (view, (&), (.~), (?~))
import Network.HTTP.Client (HttpException (HttpExceptionRequest), managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq
  ( defaults
  , getWith
  , manager
  , param
  , partBS
  , partContentType
  , partFileName
  , partText
  , post
  , responseBody
  )
import Telegram.Bot.Api.Types
  ( EditMessageRequest (..)
  , EditMessageReplyMarkupRequest (..)
  , SendMessageRequest (..)
  , SendPhotoRequest (..)
  , TelegramResponse (TelegramResponse)
  , Token (Token)
  , Update (updateId)
  )

tshow :: Show a => a -> Text
tshow = T.pack . show

class (Monad m) => TelegramClient m where
  getUpdate :: m (Maybe Update)
  sendMessage :: SendMessageRequest -> m ()
  editMessage :: EditMessageRequest -> m ()
  editMessageReplyMarkup :: EditMessageReplyMarkupRequest -> m ()
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
                (tlsManagerSettings
                   { managerResponseTimeout = responseTimeoutMicro 35000000
                   })
        TelegramConfig tgBaseUrl (Token token) _ = telegramCfg config
    let call = view responseBody <$> getWith opts (T.unpack (T.concat [tgBaseUrl, token, "/getUpdates"]))
    responseMB <- liftIO $
      catch (Just <$> call) $
        \(HttpExceptionRequest _req _content) -> do
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
            void $ liftIO $ swapMVar lastTelegramUpdateId $ updateId x
            pure $ Just x

  sendMessage SendMessageRequest {..} = do
    TelegramConfig tgBaseUrl (Token token) _ <- asks $ telegramCfg . config
    let partChatId = partText "chat_id" $ tshow mChatId
        partTxt = partText "text" mText
        partDisableNotification = partText "disable_notification" $ (T.pack . show) mDisableNotification
        partReplyTo = maybeToList $ partText "reply_to_message_id" . T.pack . show <$> mReplyToMsgId
        partParseMode = partText "parse_mode" "HTML"
        partKb = maybeToList $ partText "reply_markup" . decodeUtf8 . BL.toStrict . encode <$> mInlineKeyboard
    void $ liftIO $ post (T.unpack (T.concat [tgBaseUrl, token, "/sendMessage"])) $ [partChatId, partTxt, partDisableNotification, partParseMode] ++ partReplyTo ++ partKb

  editMessage EditMessageRequest {..} = do
    TelegramConfig tgBaseUrl (Token token) _ <- asks $ telegramCfg . config
    let partChatId = partText "chat_id" $ tshow eChatId
        partMsgId = partText "message_id" $ tshow eMessageId
        partTxt = partText "text" eText
        partParseMode = partText "parse_mode" "HTML"
        partKb = maybeToList $ partText "reply_markup" . decodeUtf8 . BL.toStrict . encode <$> eInlineKeyboard
    void $ liftIO $ post (T.unpack (T.concat [tgBaseUrl, token, "/editMessageText"])) $ [partChatId, partMsgId, partTxt, partParseMode] ++ partKb

  editMessageReplyMarkup EditMessageReplyMarkupRequest {..} = do
    TelegramConfig tgBaseUrl (Token token) _ <- asks $ telegramCfg . config
    let partChatId = partText "chat_id" $ tshow rChatId
        partMsgId = partText "message_id" $ tshow rMessageId
        partKb = maybeToList $ partText "reply_markup" . decodeUtf8 . BL.toStrict . encode <$> rInlineKeyboard
    void $ liftIO $ post (T.unpack (T.concat [tgBaseUrl, token, "/editMessageReplyMarkup"])) $ [partChatId, partMsgId] ++ partKb

  sendPhoto SendPhotoRequest {..} = do
    TelegramConfig tgBaseUrl (Token token) _ <- asks $ telegramCfg . config
    let partChatId = partText "chat_id" $ tshow pChatId
        -- partCaption = partText "caption" want
        partPhoto = partBS "photo" content & (partFileName ?~ "photo.png") & (partContentType ?~ "image/png")
        partKb = maybeToList $ partText "reply_markup" . decodeUtf8 . BL.toStrict . encode <$> pInlineKeyboard
    void $ liftIO $ post (T.unpack (T.concat [tgBaseUrl, token, "/sendPhoto"])) $ [partChatId, partPhoto] ++ partKb
