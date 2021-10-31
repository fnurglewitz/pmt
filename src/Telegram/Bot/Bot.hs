{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Bot (startBot) where

import App.Config
import qualified App.Database as DB
import App.Logging
import App.Monad
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception
import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (..),
    forever,
    runExceptT,
  )
import Control.Monad.Reader
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (find, intersperse, nub, splitAt)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import PoD.Api
import PoD.Parser
import PoD.Types (Hit (Hit, _characterName, _createdAt, _difficulty, _iQuality, _iType, _itemJson, _itemProperties, _lastInGame, _lastOnline, _lvlReq, _name, _note, _prf, _qualityCode, _tradeId, _userId, _username), ItemProperty (..), SearchResponse (SearchResponse, _hits))
import System.Log.FastLogger
import Telegram.Bot.Api.Client
import Telegram.Bot.Api.Types
import Telegram.Bot.Auth (checkAuth)
import Utils.Tabular (priceCheckTable)

startBot :: AppCtx Connection -> IO ()
startBot ctx@AppCtx {..} = do
  logGeneric logger "INFO" config "Starting telegram bot"
  forever $ do
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
    Nothing -> throwError "No update found, fuck you"
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
handleMessage m@Message {..} = do
  case from of
    Nothing -> return ()
    Just user -> do
      auth <- checkAuth user
      case mbCommand of
        Just cmd -> handleCommand cmd auth m
        Nothing -> return ()
  where
    mbCommand = getEntity "bot_command" m >> text <&> head . T.words

handleCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Text -> DB.Auth -> Message -> m ()
handleCommand "/start" a m = startCommand m
-- Track requests
handleCommand "/track" a m = trackCommand a m
handleCommand "/list" a m = listTrackCommand a m
-- Price checks
handleCommand "/addpc" a m = addPriceCheckCommand a m
handleCommand "/pc" a m = priceCheckCommand m
handleCommand "/delpc" a m = deletePriceCheckCommand m
handleCommand "/listpc" a m = listPriceCheckCommand a m
handleCommand "/confpc" a m = configurePriceCheckCommand m
-- none
handleCommand cmd _ m@Message {..} = replyToMessage m (T.append "Unknown command: " cmd)

startCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
startCommand m@Message {..} = do
  mbAuth <- DB.getAuth uid
  case mbAuth of
    Just _ -> return () -- already on db, no need to do anything
    Nothing -> do
      now <- liftIO getCurrentTime
      DB.saveAuth $ DB.Auth uid now False 0 0 uname -- TODO
  where
    uid = maybe 0 userId from
    uname = fromMaybe "" (from >>= username)

trackCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => DB.Auth -> Message -> m ()
trackCommand DB.Auth {..} m@Message {..} = case urlEntity of
  Nothing -> replyToMessage m "No PoD url provided"
  Just (MessageEntity _ offset len _ _ _) -> do
    let url = substr (fromInteger offset) (fromInteger len) (fromMaybe "" text)
        rawnotes = T.drop (fromIntegral $ offset + len) (fromMaybe "" text)
        notes = if T.length rawnotes == 0 then "" else T.drop 1 rawnotes
    case parsePodUri url of
      Left err -> replyToMessage m (T.append "Could not parse url: " err)
      Right query -> do
        trLen <- length <$> DB.listTrackRequest userIdTxt
        if trLen >= fromIntegral aMaxTrackRequests
          then do
            replyToMessage m $ "Maximum number of track requests reached (" `T.append` (T.pack . show $ aMaxTrackRequests) `T.append` "). Track request denied."
          else do
            now <- liftIO getCurrentTime
            DB.saveTrackRequest $ DB.TrackRequest Nothing userIdTxt url query notes False now now
            replyToMessage m "Track request accepted"
  where
    urlEntity = getEntity "url" m
    substr o l t = T.take l $ T.drop o t
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit

listTrackCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => DB.Auth -> Message -> m ()
listTrackCommand DB.Auth {..} m@Message {..} = do
  t <- DB.listTrackRequest $ (T.pack . show) (maybe 0 userId from)
  let msgTxt = Prelude.foldl T.append ("Track requests (" `T.append` (T.pack . show $ length t) `T.append` "/" `T.append` (T.pack . show $ aMaxTrackRequests) `T.append` "):\n") (toListElem <$> t)
  replyToMessageWithKeyboard m msgTxt (InlineKeyboardMarkup $ listToMatrix 2 $ toKeyboardBtn <$> t)
  where
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit
    toListElem DB.TrackRequest {..} = (T.pack . show $ fromMaybe (-1) requestId) `T.append` " - " `T.append` notes `T.append` "\n"
    toKeyboardBtn DB.TrackRequest {..} = InlineKeyboardButton (T.append "delete: " notes) Nothing (Just ("stopTracking," `T.append` userId `T.append` "," `T.append` (T.pack . show $ fromMaybe 0 requestId)))

priceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
priceCheckCommand m@Message {..} = do
  ctx@(AppCtx (Config _ _ _ _ _ _ RenderConfig {..}) _ _ _ font) <- ask
  case text >>= listToMaybe . drop 1 . T.words of
    Nothing -> replyToMessage m "No price check name provided"
    Just pcName -> do
      mbPc <- DB.findPc userIdTxt pcName
      case mbPc of
        Nothing -> replyToMessage m (T.append "No price check found with name: " pcName)
        Just DB.PriceCheck {..} -> do
          SearchResponse {..} <- liftIO $ doSearch pcSearchQuery
          let msg = "<pre>" `T.append` (priceCheckTable $ hitToRow (fromMaybe "" pcConfig) <$> _hits) `T.append` "</pre>"
          replyToMessage m msg
  where
    hitToRow cfg hit@Hit {..} = (_username, [applyConfig cfg hit, _note])
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit

addPriceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => DB.Auth -> Message -> m ()
addPriceCheckCommand DB.Auth {..} m@Message {..} = case urlEntity of
  Nothing -> replyToMessage m "No PoD url provided"
  Just (MessageEntity _ offset len _ _ _) -> do
    let url = substr (fromInteger offset) (fromInteger len) (fromMaybe "" text)
        rawnotes = T.drop (fromIntegral $ offset + len) (fromMaybe "" text)
        notes = if T.length rawnotes == 0 then "" else T.drop 1 rawnotes
    case parsePodUri url of
      Left err -> replyToMessage m (T.append "Could not parse url: " err)
      Right query -> do
        pcLen <- length <$> DB.listPc userIdTxt
        if pcLen >= fromIntegral aMaxPriceChecks
          then do
            replyToMessage m $ "Maximum number of price checks reached (" `T.append` (T.pack . show $ aMaxPriceChecks) `T.append` "). Price check denied."
          else do
            DB.savePc $ DB.PriceCheck Nothing userIdTxt notes url query Nothing
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

listPriceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => DB.Auth -> Message -> m ()
listPriceCheckCommand DB.Auth {..} m@Message {..} = do
  t <- DB.listPc $ (T.pack . show) (maybe 0 userId from)
  let msgTxt = Prelude.foldl T.append ("Price checks (" `T.append` (T.pack . show $ length t) `T.append` "/" `T.append` (T.pack . show $ aMaxPriceChecks) `T.append` "):\n") (toListElem <$> t)
  replyToMessageWithKeyboard m msgTxt (InlineKeyboardMarkup $ listToMatrix 2 $ toKeyboardBtn <$> t)
  where
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit
    toListElem DB.PriceCheck {..} = (T.pack . show $ fromMaybe (-1) pcId) `T.append` ". <a href=\"" `T.append` pcUrl `T.append` "\">" `T.append` pcName `T.append` "</a>\n"
    toKeyboardBtn DB.PriceCheck {..} = InlineKeyboardButton (T.append "delete: " pcName) Nothing (Just ("deletePc," `T.append` pcUserId `T.append` "," `T.append` pcName))

configurePriceCheckCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Message -> m ()
configurePriceCheckCommand m@Message {..} = do
  ctx@(AppCtx (Config _ _ _ _ _ _ RenderConfig {..}) _ _ _ font) <- ask
  case text >>= listToMaybe . drop 1 . T.words of
    Nothing -> replyToMessage m "No price check name provided"
    Just pcName -> do
      mbPc <- DB.findPc userIdTxt pcName
      case mbPc of
        Nothing -> replyToMessage m (T.append "No price check found with name: " pcName)
        Just DB.PriceCheck {..} -> do
          SearchResponse {..} <- liftIO $ doSearch pcSearchQuery
          let txtToBtn (code, label) = InlineKeyboardButton {btnText = label, btnUrl = Nothing, btnCbkData = Just ("confPc," `T.append` pcUserId `T.append` "," `T.append` pcName `T.append` "," `T.append` code)}
              resetBtnRow = [InlineKeyboardButton {btnText = "RESET", btnUrl = Nothing, btnCbkData = Just ("confPc," `T.append` pcUserId `T.append` "," `T.append` pcName `T.append` "," `T.append` "RESET")}]
              props = txtToBtn <$> (nub . concat $ hitToProps <$> _hits)
              curCfg = fromMaybe "- Not configured -" pcConfig
          replyToMessageWithKeyboard m ("Current configuration:\n" `T.append` curCfg) (InlineKeyboardMarkup $ resetBtnRow : listToMatrix 2 props)
  where
    hitToProps Hit {..} = ((,) <$> _itemPropertyCode <*> _itemPropertyLabel) <$> filter (\ItemProperty {..} -> isJust _itemPropertyValue) _itemProperties
    userIdTxt = (T.pack . show) (maybe 0 userId from) -- TODO: nice shit

handleCallback :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => CallbackQuery -> m ()
handleCallback cbk@CallbackQuery {..} = do
  case cbkData of
    Nothing -> logError "empty callback data" -- TODO
    Just dat -> do
      let splitted = T.splitOn "," dat
          command = fromMaybe "" $ listToMaybe splitted
      handleCallbackCommand command cbk splitted
  where
    uid = T.pack . show $ userId cbkFrom

handleCallbackCommand :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => Text -> CallbackQuery -> [Text] -> m ()
handleCallbackCommand "deletePc" cbk params = deletePriceCheckCallback cbk params
handleCallbackCommand "confPc" cbk params = configurePriceCheckCallback cbk params
handleCallbackCommand "stopTracking" cbk params = deleteTrackRequestCallback cbk params
handleCallbackCommand cmd _ _ = logError $ "Unknown callback command: " `T.append` cmd

deletePriceCheckCallback :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => CallbackQuery -> [Text] -> m ()
deletePriceCheckCallback _ params = do
  if length params /= 3
    then logError ("wrong number of parameters (" `T.append` (T.pack . show $ length params) `T.append` ") for command deletePc")
    else
      let uid = params !! 1
          pcName = params !! 2
       in DB.deletePc uid pcName

configurePriceCheckCallback :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => CallbackQuery -> [Text] -> m ()
configurePriceCheckCallback cbk@CallbackQuery {..} params = do
  if length params /= 4
    then logError ("wrong number of parameters (" `T.append` (T.pack . show $ length params) `T.append` ") for command deletePc")
    else do
      let uid = params !! 1
          pcName = params !! 2
          code = params !! 3
      mbPc <- DB.findPc uid pcName
      case mbPc of
        Nothing -> logError (T.append "No price check found with name: " pcName)
        Just DB.PriceCheck {..} -> do
          let Chat {..} = maybe (Chat 0 "") chat cbkMessage
              msgId = maybe 0 messageId cbkMessage
              oldKb = replyMarkup =<< cbkMessage
              newConf = updateCfg code pcConfig
          DB.configurePc uid pcName newConf
          editMessage $ EditMessageRequest (T.pack . show $ chatId) (T.pack . show $ msgId) ("Current configuration:\n" `T.append` fromMaybe "- Not configured -" newConf) oldKb
  where
    updateCfg :: Text -> Maybe Text -> Maybe Text
    updateCfg "RESET" _ = Nothing
    updateCfg code Nothing = Just code
    updateCfg code (Just old) = Just $ old `T.append` "/" `T.append` code

deleteTrackRequestCallback :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => CallbackQuery -> [Text] -> m ()
deleteTrackRequestCallback cbk params = do
  if length params /= 3
    then logError ("wrong number of parameters (" `T.append` (T.pack . show $ length params) `T.append` ") for command stopTracking")
    else
      let uid = params !! 1
          trId = params !! 2
       in DB.deleteTrackRequest uid (readDecimal trId)
  where
    readDecimal = T.foldl' (\a c -> a * 10 + toInteger (digitToInt c)) 0

getEntity :: Text -> Message -> Maybe MessageEntity
getEntity entityType (Message _ _ _ _ _ (Just entities) _) = find (\MessageEntity {eType = t} -> t == entityType) entities
getEntity _ _ = Nothing

replyToMessage m@Message {..} txt = sendMessage $ SendMessageRequest (T.pack . show $ chatId chat) txt True (Just messageId) Nothing

replyToMessageWithKeyboard m@Message {..} txt keyboard = sendMessage $ SendMessageRequest (T.pack . show $ chatId chat) txt True (Just messageId) (Just keyboard)

listToMatrix :: Int -> [a] -> [[a]]
listToMatrix _ [] = []
listToMatrix n xs = row : listToMatrix n remaining
  where
    (row, remaining) = splitAt n xs

-- TODO: redo this shit
applyConfig :: Text -> Hit -> Text
applyConfig cfg Hit {..} =
  let codes = T.splitOn "/" cfg
   in T.intercalate "/" $ go codes _itemProperties
  where
    go :: [Text] -> [ItemProperty] -> [Text]
    go [] _ = []
    go [code] ps = filter (/= T.empty) [fromMaybe "" $ _itemPropertyValue =<< find (\ItemProperty {..} -> code == _itemPropertyCode) ps]
    go (c : cs) ps = go [c] ps ++ go cs ps
