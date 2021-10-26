{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Api.Types where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Null),
    object,
    withObject,
    (.:),
    (.:?),
  )
import qualified Data.ByteString as B
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import GHC.Generics (Generic)
import Utils.Aeson (removeFieldLabelPrefix)

newtype Token = Token Text deriving (Show)

instance IsString Token where
  fromString = Token . T.pack

data TelegramResponse a = TelegramResponse
  { ok :: Bool,
    result :: a
  }
  deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (TelegramResponse a) where
  parseJSON = withObject "TgResponse" $ \v ->
    TelegramResponse
      <$> v .: "ok"
      <*> v .: "result"

instance (ToJSON a) => ToJSON (TelegramResponse a) where
  toJSON (TelegramResponse ok res) =
    object ["ok" .= ok, "result" .= res]

data Update = Update
  { updateId :: Integer,
    message :: Maybe Message,
    callbackQuery :: Maybe CallbackQuery
  }
  deriving (Eq, Show, Generic)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \v ->
    Update
      <$> v .: "update_id"
      <*> v .:? "message"
      <*> v .:? "callback_query"

instance ToJSON Update where
  toJSON (Update uid msg cbk) =
    object $ Prelude.filter ((/= Null) . snd) ["update_id" .= uid, "message" .= msg, "callback_query" .= cbk]

data CallbackQuery = CallbackQuery
  { cbkId :: Text,
    cbkFrom :: User,
    cbkMessage :: Maybe Message,
    cbkData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON = withObject "CallbackQuery" $ \v ->
    CallbackQuery
      <$> v .: "id"
      <*> v .: "from"
      <*> v .:? "message"
      <*> v .:? "data"

instance ToJSON CallbackQuery where
  toJSON CallbackQuery {..} =
    object $ Prelude.filter ((/= Null) . snd) ["id" .= cbkId, "from" .= cbkFrom, "message" .= cbkMessage, "data" .= cbkData]

data Message = Message
  { messageId :: Integer,
    from :: Maybe User,
    date :: Integer,
    chat :: Chat,
    text :: Maybe Text,
    entities :: Maybe [MessageEntity]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "message_id"
      <*> v .:? "from"
      <*> v .: "date"
      <*> v .: "chat"
      <*> v .:? "text"
      <*> v .:? "entities"

instance ToJSON Message where
  toJSON Message {..} =
    object $ Prelude.filter ((/= Null) . snd) ["message_id" .= messageId, "from" .= from, "date" .= date, "chat" .= chat, "text" .= text, "entities" .= entities]

data MessageEntity = MessageEntity
  { eType :: Text,
    eOffset :: Integer,
    eLength :: Integer,
    eUrl :: Maybe Text,
    eUser :: Maybe User,
    eLanguage :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON MessageEntity where
  parseJSON = withObject "MessageEntity" $ \v ->
    MessageEntity
      <$> v .: "type"
      <*> v .: "offset"
      <*> v .: "length"
      <*> v .:? "url"
      <*> v .:? "user"
      <*> v .:? "language"

instance ToJSON MessageEntity where
  toJSON MessageEntity {..} =
    object $ Prelude.filter ((/= Null) . snd) ["type" .= eType, "offset" .= eOffset, "length" .= eLength, "url" .= eUrl, "user" .= eUser, "language" .= eLanguage]

data User = User
  { userId :: Integer,
    isBot :: Bool,
    firstName :: Text,
    lastName :: Maybe Text,
    username :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v .: "id"
      <*> v .: "is_bot"
      <*> v .: "first_name"
      <*> v .:? "last_name"
      <*> v .:? "username"

instance ToJSON User where
  toJSON User {..} =
    object $ Prelude.filter ((/= Null) . snd) ["id" .= userId, "is_bot" .= isBot, "first_name" .= firstName, "last_name" .= lastName, "username" .= username]

data Chat = Chat
  { chatId :: Integer,
    chatType :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \v ->
    Chat
      <$> v .: "id"
      <*> v .: "type"

instance ToJSON Chat where
  toJSON Chat {..} =
    object $ Prelude.filter ((/= Null) . snd) ["id" .= chatId, "type" .= chatType]

data SendPhotoRequest = SendPhotoRequest
  { pChatId :: Text,
    content :: B.ByteString,
    want :: Text,
    url :: Text,
    tradeId :: Text
  }
  deriving (Eq, Show)

data SendMessageRequest = SendMessageRequest
  { mChatId :: Text,
    mText :: Text,
    mDisableNotification :: Bool,
    mReplyToMsgId :: Maybe Integer,
    mInlineKeyboard :: Maybe InlineKeyboardMarkup
  }
  deriving (Eq, Show)

data InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboard :: [[InlineKeyboardButton]]
  }
  deriving (Eq, Show, Generic)

instance ToJSON InlineKeyboardMarkup where
  toJSON (InlineKeyboardMarkup ik) =
    object ["inline_keyboard" .= ik]

data InlineKeyboardButton = InlineKeyboardButton
  { btnText :: Text,
    btnUrl :: Maybe Text,
    btnCbkData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton txt url dat) =
    object $ Prelude.filter ((/= Null) . snd) ["text" .= txt, "url" .= url, "callback_data" .= dat]
