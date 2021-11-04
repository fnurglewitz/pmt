{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Telegram.Bot.Bot where

import App.Config ( AppCtx (..) )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
import App.Database (Auth (..), DB)
import qualified App.Database as DB
import App.Logging (HasLogger (logError), logGeneric)
import App.Monad
import Control.Concurrent (threadDelay)
import Control.Monad.Except
  ( MonadError (throwError)
  )
import Control.Monad.Reader
  ( MonadReader (ask)
  , ReaderT (runReaderT)
  , forever
  )
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Char (digitToInt)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import PoD.Api (doSearch)
import PoD.Parser (parsePodUri)
import PoD.Types
  ( Hit
      ( Hit
      , _characterName
      , _createdAt
      , _difficulty
      , _iQuality
      , _iType
      , _itemJson
      , _itemProperties
      , _lastInGame
      , _lastOnline
      , _lvlReq
      , _name
      , _note
      , _prf
      , _qualityCode
      , _tradeId
      , _userId
      , _username
      )
  , ItemProperty (..)
  , SearchResponse (SearchResponse, _hits)
  )
import Streaming
  ( Alternative ((<|>))
  , MonadIO (..)
  , MonadTrans (lift)
  , void
  )
import Telegram.Bot.Api.Client
  ( TelegramClient (editMessage, getUpdate, sendMessage)
  )
import Telegram.Bot.Api.Types
  ( CallbackQuery (..)
  , Chat (Chat, chatId, chatType)
  , EditMessageRequest (EditMessageRequest)
  , InlineKeyboardButton
      ( InlineKeyboardButton
      , btnCbkData
      , btnText
      , btnUrl
      )
  , InlineKeyboardMarkup (InlineKeyboardMarkup)
  , Message (..)
  , SendMessageRequest (SendMessageRequest)
  , Update (Update)
  , User (userId)
  , getEntity
  )
import Telegram.Bot.Auth (checkAuth)
import Telegram.Monad
import Text.InterpolatedString.QM (qms)
import Utils.Tabular (priceCheckTable)

type Effects m =
  ( MonadReader (AppCtx Connection) m
  , MonadIO m
  , MonadError Text m
  , TelegramClient m
  , DB m
  , HasLogger m
  )

type Authorized m = ReaderT Auth m

tshow :: Show a => a -> Text
tshow = T.pack . show

startBot :: AppCtx Connection -> IO ()
startBot ctx@AppCtx {..} = do
  logGeneric logger "INFO" config "Starting telegram bot"
  forever $ do
    r <- runExceptT $ runReaderT (runAppM runBot) ctx
    case r of
      Left err -> logGeneric logger "ERROR" config err
      Right x -> pure x
    threadDelay 1000000

executeBotAction :: Effects m => TelegramAction -> m ()
executeBotAction (NoKeyboard (ReplyMessage Message{..} txt)) = sendMessage $ SendMessageRequest (tshow . chatId $ chat) txt True (Just messageId) Nothing
executeBotAction (WithKeyboard (ReplyMessage Message{..} txt) kb) = sendMessage $ SendMessageRequest (tshow . chatId $ chat) txt True (Just messageId) (Just kb)
executeBotAction NoAction = pure ()
executeBotAction _ = error "Unimplemented action"

runBot :: Effects m => m ()
runBot = do
  mbUpdate <- getUpdate
  case selectAction =<< mbUpdate of
    Nothing -> throwError "No update found, fuck you"
    Just x -> x
  where
    selectAction (Update _ msg cbk) = handleMessage <$> msg <|> handleCallback <$> cbk

-- already in transformers 0.6.0.0 , not here
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

handleMessage :: Effects m => Message -> m ()
handleMessage m@Message {..} = void $ runMaybeT do
  user <- hoistMaybe from
  auth <- lift $ checkAuth user
  cmd <- hoistMaybe $ getEntity "bot_command" m
  lift $ flip runReaderT auth $ handleCommand cmd m

-- handleCommand :: Effects m=> Text -> DB.Auth -> Message -> m ()
handleCommand :: Effects m => Text -> Message -> Authorized m ()
-- Track requests
handleCommand "/track" = trackCommand
handleCommand "/list" = listTrackCommand
-- Price checks
handleCommand "/addpc" = addPriceCheckCommand
handleCommand "/pc" = lift . priceCheckCommand
handleCommand "/delpc" = lift . deletePriceCheckCommand
handleCommand "/listpc" = listPriceCheckCommand
handleCommand "/confpc" = lift . configurePriceCheckCommand
-- none
handleCommand cmd = \m -> lift $ runTelegramM executeBotAction $ 
  do pure $ NoKeyboard $ ReplyMessage m [qms|Unknown command: {cmd}|]

trackCommand :: Effects m => Message -> Authorized m ()
trackCommand m@Message {..} = do
  Auth {..} <- ask
  lift $
    runTelegramM executeBotAction $ do
      url <- dieOnNothing (getEntity "url" m) $ NoKeyboard $ ReplyMessage m "No PoD url provided"
      trName <- dieOnNothing (getEntity "hashtag" m) $ NoKeyboard $ ReplyMessage m "No TrackRequest name provided"
      query <- dieOnLeft 
        do parsePodUri url
        do \err -> NoKeyboard $ ReplyMessage m [qms|Could not parse url: {err}|]
      trLen <- lift $ length <$> DB.listTrackRequest userIdTxt
      dieIf 
        do trLen >= fromIntegral aMaxTrackRequests
        do NoKeyboard $ ReplyMessage m [qms|Maximum number of track requests reached ({aMaxTrackRequests}). Track request denied.|]
      now <- liftIO getCurrentTime
      lift $ DB.saveTrackRequest $ DB.TrackRequest Nothing userIdTxt url query trName False now now
      pure $ NoKeyboard $ ReplyMessage m "Track request accepted"
  where
    userIdTxt = tshow (maybe 0 userId from) -- TODO: nice shit

listTrackCommand :: Effects m => Message -> Authorized m ()
listTrackCommand m@Message {..} = do
  Auth {..} <- ask
  lift $
    runTelegramM executeBotAction $ do
      t <- lift $ DB.listTrackRequest $ tshow (maybe 0 userId from)
      let msgTxt = [qms|Track requests ({length t}/{aMaxTrackRequests}):\n|] <> mconcat (toListElem <$> t)
      pure $ WithKeyboard (ReplyMessage m msgTxt) (InlineKeyboardMarkup $ listToMatrix 2 $ toKeyboardBtn <$> t)
    where
      toListElem DB.TrackRequest {..} = [qms|{fromMaybe (-1) requestId} - {notes}\n|]
      toKeyboardBtn DB.TrackRequest {..} =
        InlineKeyboardButton
          do [qms|delete: {notes}|]
          do Nothing
          do Just [qms|stopTracking,{userId},{fromMaybe 0 requestId}|]

priceCheckCommand :: Effects m => Message -> m ()
priceCheckCommand m@Message {..} = runTelegramM executeBotAction $ do
      pcName' <- dieOnNothing (text >>= listToMaybe . drop 1 . T.words) (NoKeyboard $ ReplyMessage m "No price check name provided")
      mbPc <- lift $ DB.findPc userIdTxt pcName'
      DB.PriceCheck {..} <- dieOnNothing mbPc $ NoKeyboard $ ReplyMessage m [qms|No price check found with name: {pcName'}|]
      SearchResponse {..} <- liftIO $ doSearch pcSearchQuery
      let msg = "<pre>" <> priceCheckTable (hitToRow (fromMaybe "" pcConfig) <$> _hits) <> "</pre>"
      pure $ NoKeyboard $ ReplyMessage m msg
  where
    hitToRow cfg hit@Hit {..} = (_username, [applyConfig cfg hit, _note])
    userIdTxt = tshow (maybe 0 userId from) -- TODO: nice shit

addPriceCheckCommand :: Effects m => Message -> Authorized m ()
addPriceCheckCommand m@Message {..} = do
  Auth {..} <- ask
  lift $
    runTelegramM executeBotAction $ do
      url <- dieOnNothing (getEntity "url" m) $ NoKeyboard $ ReplyMessage m "No PoD url provided"
      pcName <- dieOnNothing (getEntity "hashtag" m) $ NoKeyboard $ ReplyMessage m "No PriceCheck name provided"
      query <- dieOnLeft (parsePodUri url) $ \err -> NoKeyboard $ ReplyMessage m [qms|Could not parse url: {err}|]
      pcLen <- lift $ length <$> DB.listPc userIdTxt
      dieIf
        do pcLen >= fromIntegral aMaxPriceChecks 
        do NoKeyboard $ ReplyMessage m [qms|Maximum number of price checks reached ({aMaxPriceChecks}). Price check denied.|]
      lift $ DB.savePc $ DB.PriceCheck Nothing userIdTxt pcName url query Nothing
      pure $ NoKeyboard $ ReplyMessage m "PriceCheck saved"
  where
    userIdTxt = tshow (maybe 0 userId from) -- TODO: nice shit

deletePriceCheckCommand :: Effects m => Message -> m ()
deletePriceCheckCommand m@Message {..} = do
  runTelegramM executeBotAction $ do
    pcName <- dieOnNothing (text >>= listToMaybe . drop 1 . T.words) (NoKeyboard $ ReplyMessage m "No price check name provided")
    mbPc <- lift $ DB.findPc userIdTxt pcName
    void $ dieOnNothing mbPc (NoKeyboard $ ReplyMessage m [qms|No price check found with name: {pcName}|])
    lift $ DB.deletePc userIdTxt pcName
    pure $ NoKeyboard $ ReplyMessage m "PriceCheck deleted"
  where
    userIdTxt = tshow (maybe 0 userId from) -- TODO: nice shit

listPriceCheckCommand :: Effects m => Message -> Authorized m ()
listPriceCheckCommand m@Message {..} = do
  Auth {..} <- ask
  lift $ 
    runTelegramM executeBotAction $ do
      t <- lift $ DB.listPc $ tshow (maybe 0 userId from)
      let msgTxt = [qms|Price checks ({length t}/{aMaxPriceChecks}):\n|] <> mconcat (toListElem <$> t)
      pure $ WithKeyboard (ReplyMessage m msgTxt) (InlineKeyboardMarkup $ listToMatrix 2 $ toKeyboardBtn <$> t)
  where
    toListElem DB.PriceCheck {..} = [qms|<a href="{pcUrl}">{pcName}</a>\n|]
    toKeyboardBtn DB.PriceCheck {..} = InlineKeyboardButton [qms|delete:{pcName}|] Nothing (Just [qms|deletePc,{pcUserId},{pcName}|])

configurePriceCheckCommand :: Effects m => Message -> m ()
configurePriceCheckCommand m@Message {..} = do
  runTelegramM executeBotAction $ do
    pcName' <- dieOnNothing (text >>= listToMaybe . drop 1 . T.words) (NoKeyboard $ ReplyMessage m "No price check name provided")
    mbPc <- lift $ DB.findPc userIdTxt pcName'
    DB.PriceCheck {..} <- dieOnNothing mbPc (NoKeyboard $ ReplyMessage m [qms|No price check found with name: {pcName'}|])    
    SearchResponse {..} <- liftIO $ doSearch pcSearchQuery
    let txtToBtn :: (Text, Text) -> InlineKeyboardButton -- needed, otherwise qms cannot understand that code is Text
        txtToBtn (code, label) =
          InlineKeyboardButton
            { btnText = label
            , btnUrl = Nothing
            , btnCbkData = Just [qms|confPc,{pcUserId},{pcName},{code}|]
            }
        resetBtnRow =
          [ InlineKeyboardButton
              { btnText = "RESET"
              , btnUrl = Nothing
              , btnCbkData = Just [qms|confPc,{pcUserId},{pcName},RESET|]
              }
          ]
        props = txtToBtn <$> (nub . concat $ hitToProps <$> _hits)
        curCfg = fromMaybe "- Not configured -" pcConfig
    pure $ WithKeyboard (ReplyMessage m [qms|Current configuration:\n{curCfg}|]) (InlineKeyboardMarkup $ resetBtnRow : listToMatrix 2 props)
  where
    hitToProps Hit {..} = ((,) <$> _itemPropertyCode <*> _itemPropertyLabel) <$> filter (\ItemProperty {..} -> isJust _itemPropertyValue) _itemProperties
    userIdTxt = tshow (maybe 0 userId from) -- TODO: nice shit

handleCallback :: Effects m => CallbackQuery -> m ()
handleCallback cbk@CallbackQuery {..} = do
  case cbkData of
    Nothing -> logError "empty callback data" -- TODO
    Just dat -> do
      let splitted = T.splitOn "," dat
          command = fromMaybe "" $ listToMaybe splitted
      handleCallbackCommand command cbk splitted

handleCallbackCommand :: Effects m => Text -> CallbackQuery -> [Text] -> m ()
handleCallbackCommand "deletePc" cbk params = deletePriceCheckCallback cbk params
handleCallbackCommand "confPc" cbk params = configurePriceCheckCallback cbk params
handleCallbackCommand "stopTracking" cbk params = deleteTrackRequestCallback cbk params
handleCallbackCommand cmd _ _ = logError [qms|"Unknown callback command: {cmd}|]

deletePriceCheckCallback :: Effects m => CallbackQuery -> [Text] -> m ()
deletePriceCheckCallback _ params = do
  if length params /= 3
    then logError [qms|wrong number of parameters ({length params}) for command deletePc|]
    else
      let uid = params !! 1
          pcName = params !! 2
       in DB.deletePc uid pcName

configurePriceCheckCallback :: Effects m => CallbackQuery -> [Text] -> m ()
configurePriceCheckCallback CallbackQuery {..} params = do
  if length params /= 4
    then logError [qms|wrong number of parameters ({length params}) for command confPc|]
    else do
      let uid = params !! 1
          pcName' = params !! 2
          code = params !! 3
      mbPc <- DB.findPc uid pcName'
      case mbPc of
        Nothing -> logError [qms|No price check found with name: {pcName'}|]
        Just DB.PriceCheck {..} -> do
          let Chat {..} = maybe (Chat 0 "") chat cbkMessage
              msgId = maybe 0 messageId cbkMessage
              oldKb = replyMarkup =<< cbkMessage
              newConf = updateCfg code pcConfig
          DB.configurePc uid pcName newConf
          editMessage $ EditMessageRequest (tshow chatId) (tshow msgId) ("Current configuration:\n" <> fromMaybe "- Not configured -" newConf) oldKb
  where
    updateCfg :: Text -> Maybe Text -> Maybe Text
    updateCfg "RESET" _ = Nothing
    updateCfg code Nothing = Just code
    updateCfg code (Just old) = Just $ old <> "/" <> code

deleteTrackRequestCallback :: Effects m => CallbackQuery -> [Text] -> m ()
deleteTrackRequestCallback _ params = do
  if length params /= 3
    then logError [qms|wrong number of parameters ({length params}) for command stopTracking|]
    else
      let uid = params !! 1
          trId = params !! 2
       in DB.deleteTrackRequest uid (readDecimal trId)
  where
    readDecimal = T.foldl' (\a c -> a * 10 + toInteger (digitToInt c)) 0

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
