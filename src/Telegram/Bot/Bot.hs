{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Bot (startBot) where

import App.Config
import App.Database (DB (listTrackRequest))
import qualified App.Database as DB
import App.Logging
import App.Monad
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.List (find, splitAt)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import qualified Options.Applicative.Help as T
import PoD.Api
import PoD.Parser
import PoD.Types (Hit (Hit, _characterName, _createdAt, _difficulty, _iQuality, _iType, _itemJson, _itemProperties, _lastInGame, _lastOnline, _lvlReq, _name, _note, _prf, _qualityCode, _tradeId, _userId, _username), SearchResponse (SearchResponse, _hits))
import System.Log.FastLogger
import Telegram.Bot.Api.Client
import Telegram.Bot.Api.Types

startBot :: AppCtx Connection -> IO ()
startBot ctx@AppCtx {..} = forever $ do
  r <- runExceptT $ runReaderT (runAppM runBot) ctx
  case r of
    Left err -> logGeneric logger "ERROR" config err
    Right x -> pure x
  threadDelay 1000000

runBot :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => m ()
runBot = do
  AppCtx {..} <- ask
  mbUpdate <- getUpdate
  case selectAction =<< mbUpdate of
    Nothing -> logError "No update found, fuck you" -- throwError "No update found, fuck you"
    Just x -> x
  where
    selectAction (Update _ msg cbk) = (handleMessage <$> msg) <|> (handleCallback <$> cbk)

{-
runBot = do
  AppCtx {..} <- ask
  mUpdate <- getUpdate
  case mUpdate of
    Nothing -> logError "No update found, fuck you" -- throwError "No update found, fuck you"
    Just (Update uid msg cbk) -> do
      case msg of
        Just message -> handleMessage message
        Nothing -> forM_ cbk handleCallback
-}

handleMessage :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
handleMessage m@Message {..} = case mbCommand of
  Just cmd -> handleCommand cmd m
  Nothing -> return ()
  where
    mbCommand = getEntity "bot_command" m >> text <&> head . T.words

handleCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Text -> Message -> m ()
handleCommand "/track" m = trackCommand m
handleCommand "/list" m = listTrackCommand m
handleCommand "/addpc" m = addPriceCheckCommand m
handleCommand "/pc" m@Message {..} = priceCheckCommand m
handleCommand "/delpc" m@Message {..} = deletePriceCheckCommand m
handleCommand "/listpc" m@Message {..} = listPriceCheckCommand m
handleCommand cmd m@Message {..} = replyToMessage m (T.append "Unknown command: " cmd)

trackCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
trackCommand m@Message {..} = case urlEntity of
  Nothing -> replyToMessage m "No PoD url provided"
  Just (MessageEntity _ offset len _ _ _) -> do
    let url = substr (fromInteger offset) (fromInteger len) (fromMaybe "" text)
        rawnotes = T.drop (fromIntegral $ offset + len) (fromMaybe "" text)
        notes = if T.length rawnotes == 0 then "" else T.drop 1 rawnotes
    case parsePodUri url of
      Left err -> replyToMessage m (T.append "Could not parse url: " err)
      Right query -> do
        now <- liftIO getCurrentTime
        DB.saveTrackRequest $ DB.TrackRequest Nothing userIdTxt url query notes False now now
        replyToMessage m "Track request accepted"
  where
    urlEntity = getEntity "url" m
    substr o l t = T.take l $ T.drop o t
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit

listTrackCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
listTrackCommand m@Message {..} = do
  t <- listTrackRequest $ (T.pack . show) (maybe 0 userId from)
  let msgTxt = Prelude.foldl T.append "Track requests:\n" (toListElem <$> t)
  replyToMessage m msgTxt
  where
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit
    toListElem DB.TrackRequest {..} = (T.pack . show $ fromMaybe (-1) requestId) `T.append` " - " `T.append` notes `T.append` "\n"

priceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
priceCheckCommand m@Message {..} = do
  case text >>= listToMaybe . drop 1 . T.words of
    Nothing -> replyToMessage m "No price check name provided"
    Just pcName -> do
      mbPc <- DB.findPc userIdTxt pcName
      case mbPc of
        Nothing -> replyToMessage m (T.append "No price check found with name: " pcName)
        Just DB.PriceCheck {..} -> do
          SearchResponse {..} <- liftIO $ doSearch pcSearchQuery
          let msg = foldl T.append "Prices found:\n" (hitToLine <$> _hits)
          replyToMessage m msg
  where
    hitToLine Hit {..} = _username `T.append` ": " `T.append` _note `T.append` "\n"
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit

addPriceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
addPriceCheckCommand m@Message {..} = case urlEntity of
  Nothing -> replyToMessage m "No PoD url provided"
  Just (MessageEntity _ offset len _ _ _) -> do
    let url = substr (fromInteger offset) (fromInteger len) (fromMaybe "" text)
        rawnotes = T.drop (fromIntegral $ offset + len) (fromMaybe "" text)
        notes = if T.length rawnotes == 0 then "" else T.drop 1 rawnotes
    case parsePodUri url of
      Left err -> replyToMessage m (T.append "Could not parse url: " err)
      Right query -> do
        DB.savePc $ DB.PriceCheck Nothing userIdTxt notes url query
        replyToMessage m "PriceCheck saved"
  where
    urlEntity = getEntity "url" m
    substr o l t = T.take l $ T.drop o t
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit

deletePriceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
deletePriceCheckCommand m@Message {..} = do
  case text >>= listToMaybe . drop 1 . T.words of
    Nothing -> replyToMessage m "No price check name provided"
    Just pcName -> do
      mbPc <- DB.findPc userIdTxt pcName
      case mbPc of
        Nothing -> replyToMessage m (T.append "No price check found with name: " pcName)
        Just _ -> do
          DB.deletePc userIdTxt pcName
          replyToMessage m "PriceCheck deleted"
  where
    hitToLine Hit {..} = _username `T.append` ": " `T.append` _note `T.append` "\n"
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit

listPriceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
listPriceCheckCommand m@Message {..} = do
  t <- DB.listPc $ (T.pack . show) (maybe 0 userId from)
  let msgTxt = Prelude.foldl T.append "Price checks:\n" (toListElem <$> t)
  replyToMessageWithKeyboard m msgTxt (InlineKeyboardMarkup $ listToMatrix 2 $ toKeyboardBtn <$> t)
  where
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit
    toListElem DB.PriceCheck {..} = (T.pack . show $ fromMaybe (-1) pcId) `T.append` ". <a href=\"" `T.append` pcUrl `T.append` "\">" `T.append` pcName `T.append` "</a>\n"
    toKeyboardBtn DB.PriceCheck {..} = InlineKeyboardButton (T.append "deletePc: " pcName) Nothing (Just ("deletePc," `T.append` pcUserId `T.append` "," `T.append` pcName))

handleCallback :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => CallbackQuery -> m ()
handleCallback cbk@CallbackQuery {..} = do
  case cbkData of
    Nothing -> logError "empty callback data" -- TODO
    Just dat -> do
      let splitted = T.splitOn "," dat
          command = fromMaybe "" $ listToMaybe splitted
      handleCallbackCommand command splitted
  where
    uid = T.pack . show $ userId cbkFrom

handleCallbackCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Text -> [Text] -> m ()
handleCallbackCommand "deletePc" params = deletePriceCheckCallback params
handleCallbackCommand cmd _ = logError $ "Unknown callback command: " `T.append` cmd

deletePriceCheckCallback :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => [Text] -> m ()
deletePriceCheckCallback params = do
  if length params /= 3
    then logError ("wrong number of parameters (" `T.append` (T.pack . show $ length params) `T.append` ") for command deletePc")
    else
      let uid = params !! 1
          pcName = params !! 2
       in DB.deletePc uid pcName

getEntity :: Text -> Message -> Maybe MessageEntity
getEntity entityType (Message _ _ _ _ _ (Just entities)) = find (\MessageEntity {eType = t} -> t == entityType) entities
getEntity _ _ = Nothing

replyToMessage m@Message {..} txt = sendMessage $ SendMessageRequest (T.pack . show $ chatId chat) txt True (Just messageId) Nothing

replyToMessageWithKeyboard m@Message {..} txt keyboard = sendMessage $ SendMessageRequest (T.pack . show $ chatId chat) txt True (Just messageId) (Just keyboard)

listToMatrix :: Int -> [a] -> [[a]]
listToMatrix _ [] = []
listToMatrix n xs = row : listToMatrix n remaining
  where
    (row, remaining) = splitAt n xs
