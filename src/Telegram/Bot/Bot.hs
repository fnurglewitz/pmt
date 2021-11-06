{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Telegram.Bot.Bot where

import App.AppM (AppM (runAppM))
import App.Config (AppCtx (..))
import App.ReplyM (dieIf, dieOnLeft, dieOnNothing)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad.Except
  ( MonadError (throwError)
  )
import Control.Monad.Reader
  ( MonadReader (ask)
  , ReaderT (runReaderT)
  , forever
  )
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Char (digitToInt)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Types as DB
import Logging.Logger (logGeneric)
import Logging.Types (HasLogger (logError))
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
  ( TelegramClient (editMessage, editMessageReplyMarkup, getUpdate, sendMessage)
  )
import Telegram.Bot.Api.Types
  ( CallbackQuery (..)
  , Chat (Chat, chatId, chatType)
  , EditMessageReplyMarkupRequest (EditMessageReplyMarkupRequest)
  , EditMessageRequest (EditMessageRequest)
  , InlineKeyboardButton (..)
  , InlineKeyboardMarkup (InlineKeyboardMarkup, inlineKeyboard)
  , Message (..)
  , SendMessageRequest (SendMessageRequest)
  , Update (Update)
  , User (userId)
  , getEntity
  )
import Telegram.Bot.Auth (checkAuth)
import Telegram.Monad
  ( TelegramAction (..)
  , TelegramActionSimple
      ( EditMessage
      , EditMessageReplyMarkup
      , ReplyMessage
      )
  , runTelegramM
  )
import Text.InterpolatedString.QM (qms)
import Utils.Tabular (priceCheckTable)

type Effects m =
  ( MonadReader (AppCtx Connection) m
  , MonadIO m
  , MonadError Text m
  , TelegramClient m
  , DB.DB m
  , HasLogger m
  )

type Authorized m = ReaderT DB.Auth m

tshow :: Show a => a -> Text
tshow = T.pack . show

startBot :: AppCtx Connection -> IO ()
startBot ctx@AppCtx {..} = do
  logGeneric logger "INFO" config "Starting telegram bot"
  forever $ handle
    do (\e -> logGeneric logger "ERROR" config $ tshow (e :: SomeException))
    do
      r <- runExceptT $ runReaderT (runAppM runBot) ctx
      case r of
        Left err -> logGeneric logger "ERROR" config err
        Right x -> pure x
      threadDelay 1000000

executeBotAction :: Effects m => TelegramAction -> m ()
executeBotAction (NoKeyboard (ReplyMessage Message {..} txt)) = sendMessage $ SendMessageRequest (chatId chat) txt True (Just messageId) Nothing
executeBotAction (WithKeyboard (ReplyMessage Message {..} txt) kb) = sendMessage $ SendMessageRequest (chatId chat) txt True (Just messageId) (Just kb)
executeBotAction (NoKeyboard (EditMessage Message {..} txt)) = editMessage $ EditMessageRequest (chatId chat) messageId txt Nothing
executeBotAction (WithKeyboard (EditMessage Message {..} txt) kb) = editMessage $ EditMessageRequest (chatId chat) messageId txt (Just kb)
executeBotAction (NoKeyboard (EditMessageReplyMarkup Message {..})) = editMessageReplyMarkup $ EditMessageReplyMarkupRequest (chatId chat) messageId Nothing
executeBotAction (WithKeyboard (EditMessageReplyMarkup Message {..}) kb) = editMessageReplyMarkup $ EditMessageReplyMarkupRequest (chatId chat) messageId (Just kb)
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

handleCommand :: Effects m => Text -> Message -> Authorized m ()
-- Track requests
handleCommand "/track" = trackCommand
handleCommand "/list" = listTrackCommand
-- Price checks
handleCommand "/addpc" = addPriceCheckCommand
handleCommand "/pc" = lift . priceCheckCommand
handleCommand "/listpc" = listPriceCheckCommand
handleCommand "/confpc" = lift . configurePriceCheckCommand
-- none
handleCommand cmd = \m -> lift $
  runTelegramM executeBotAction $
    do pure $ NoKeyboard $ ReplyMessage m [qms|Unknown command: {cmd}|]

trackCommand :: Effects m => Message -> Authorized m ()
trackCommand m@Message {..} = do
  DB.Auth {..} <- ask
  lift $
    runTelegramM executeBotAction $ do
      url <- dieOnNothing (getEntity "url" m) $ NoKeyboard $ ReplyMessage m "No PoD url provided"
      trName <- dieOnNothing (getEntity "hashtag" m) $ NoKeyboard $ ReplyMessage m "No TrackRequest name provided"
      userId' <- dieOnNothing (userId <$> from) NoAction
      query <- dieOnLeft
        do parsePodUri url
        do \err -> NoKeyboard $ ReplyMessage m [qms|Could not parse url: {err}|]
      trLen <- lift $ length <$> DB.listTrackRequest userId'
      dieIf
        do trLen >= fromIntegral aMaxTrackRequests
        do NoKeyboard $ ReplyMessage m [qms|Maximum number of track requests reached ({aMaxTrackRequests}). Track request denied.|]
      now <- liftIO getCurrentTime
      lift $ DB.saveTrackRequest $ DB.TrackRequest Nothing userId' trName url False query now now
      pure $ NoKeyboard $ ReplyMessage m "Track request accepted"

listTrackCommand :: Effects m => Message -> Authorized m ()
listTrackCommand m@Message {..} = do
  DB.Auth {..} <- ask
  lift $
    runTelegramM executeBotAction $ do
      userId' <- dieOnNothing (userId <$> from) NoAction
      t <- lift $ DB.listTrackRequest userId'
      let msgTxt = [qms|Track requests ({length t}/{aMaxTrackRequests}):\n|] <> mconcat (trToListElem <$> t)
      pure $ WithKeyboard (ReplyMessage m msgTxt) (InlineKeyboardMarkup $ listToMatrix 2 $ trToDeleteKeyboardBtn <$> t)

priceCheckCommand :: Effects m => Message -> m ()
priceCheckCommand m@Message {..} = runTelegramM executeBotAction $ do
  pcName' <- dieOnNothing (text >>= listToMaybe . drop 1 . T.words) (NoKeyboard $ ReplyMessage m "No price check name provided")
  userId' <- dieOnNothing (userId <$> from) NoAction
  mbPc <- lift $ DB.findPc userId' pcName'
  DB.PriceCheck {..} <- dieOnNothing mbPc $ NoKeyboard $ ReplyMessage m [qms|No price check found with name: {pcName'}|]
  SearchResponse {..} <- liftIO $ doSearch pcSearchQuery
  let msg = "<pre>" <> priceCheckTable (hitToRow (fromMaybe "" pcConfig) <$> _hits) <> "</pre>"
  pure $ NoKeyboard $ ReplyMessage m msg
  where
    hitToRow cfg hit@Hit {..} = (_username, [applyConfig cfg hit, _note])

addPriceCheckCommand :: Effects m => Message -> Authorized m ()
addPriceCheckCommand m@Message {..} = do
  DB.Auth {..} <- ask
  lift $
    runTelegramM executeBotAction $ do
      url <- dieOnNothing (getEntity "url" m) $ NoKeyboard $ ReplyMessage m "No PoD url provided"
      pcName <- dieOnNothing (getEntity "hashtag" m) $ NoKeyboard $ ReplyMessage m "No PriceCheck name provided"
      query <- dieOnLeft (parsePodUri url) $ \err -> NoKeyboard $ ReplyMessage m [qms|Could not parse url: {err}|]
      userId' <- dieOnNothing (userId <$> from) NoAction
      pcLen <- lift $ length <$> DB.listPc userId'
      dieIf
        do pcLen >= fromIntegral aMaxPriceChecks
        do NoKeyboard $ ReplyMessage m [qms|Maximum number of price checks reached ({aMaxPriceChecks}). Price check denied.|]
      now <- liftIO getCurrentTime
      lift $ DB.savePc $ DB.PriceCheck Nothing userId' pcName url query Nothing now
      pure $ NoKeyboard $ ReplyMessage m "PriceCheck saved"

listPriceCheckCommand :: Effects m => Message -> Authorized m ()
listPriceCheckCommand m@Message {..} = do
  DB.Auth {..} <- ask
  lift $
    runTelegramM executeBotAction $ do
      userId' <- dieOnNothing (userId <$> from) NoAction
      t <- lift $ DB.listPc userId'
      let msgTxt = [qms|Price checks ({length t}/{aMaxPriceChecks}):\n|] <> mconcat (pcToListElem <$> t)
      pure $ WithKeyboard (ReplyMessage m msgTxt) (InlineKeyboardMarkup $ listToMatrix 2 $ pcToDeleteKeyboardBtn <$> t)

configurePriceCheckCommand :: Effects m => Message -> m ()
configurePriceCheckCommand m@Message {..} = do
  runTelegramM executeBotAction $ do
    pcName' <- dieOnNothing (text >>= listToMaybe . drop 1 . T.words) (NoKeyboard $ ReplyMessage m "No price check name provided")
    userId' <- dieOnNothing (userId <$> from) NoAction
    mbPc <- lift $ DB.findPc userId' pcName'
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
handleCallbackCommand "deleteTr" cbk params = deleteTrackRequestCallback cbk params
handleCallbackCommand "stopTracking" cbk params = stopTrackRequestCallback cbk params
handleCallbackCommand cmd _ _ = logError [qms|"Unknown callback command: {cmd}|]

deletePriceCheckCallback :: Effects m => CallbackQuery -> [Text] -> m ()
deletePriceCheckCallback CallbackQuery {..} params = do
  if length params /= 3
    then logError [qms|wrong number of parameters ({length params}) for command deletePc|]
    else do
      runTelegramM executeBotAction $ do
        let uid = readDecimal $ params !! 1
            pcName = params !! 2
        lift . void $ DB.deletePc uid pcName
        t <- lift $ DB.listPc uid
        mbAuth <- lift $ DB.getAuth uid
        DB.Auth {..} <- dieOnNothing mbAuth NoAction
        msg <- dieOnNothing cbkMessage NoAction
        let msgTxt = [qms|Price checks ({length t}/{aMaxPriceChecks}):\n|] <> mconcat (pcToListElem <$> t)
            kb = InlineKeyboardMarkup $ listToMatrix 2 $ pcToDeleteKeyboardBtn <$> t
        pure $ WithKeyboard (EditMessage msg msgTxt) kb

configurePriceCheckCallback :: Effects m => CallbackQuery -> [Text] -> m ()
configurePriceCheckCallback CallbackQuery {..} params = do
  if length params /= 4
    then logError [qms|wrong number of parameters ({length params}) for command confPc|]
    else do
      let uid = readDecimal $ params !! 1
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
          editMessage $ EditMessageRequest chatId msgId ("Current configuration:\n" <> fromMaybe "- Not configured -" newConf) oldKb
  where
    updateCfg :: Text -> Maybe Text -> Maybe Text
    updateCfg "RESET" _ = Nothing
    updateCfg code Nothing = Just code
    updateCfg code (Just old) = Just $ old <> "/" <> code

deleteTrackRequestCallback :: Effects m => CallbackQuery -> [Text] -> m ()
deleteTrackRequestCallback CallbackQuery {..} params = do
  if length params /= 3
    then logError [qms|wrong number of parameters ({length params}) for command stopTracking|]
    else do
      runTelegramM executeBotAction $ do
        let uid = readDecimal $ params !! 1
            trId = readDecimal $ params !! 2
        lift . void $ DB.deleteTrackRequest uid trId
        t <- lift $ DB.listTrackRequest uid
        mbAuth <- lift $ DB.getAuth uid
        DB.Auth {..} <- dieOnNothing mbAuth NoAction
        msg <- dieOnNothing cbkMessage NoAction
        let msgTxt = [qms|Track requests ({length t}/{aMaxTrackRequests}):\n|] <> mconcat (trToListElem <$> t)
            kb = InlineKeyboardMarkup $ listToMatrix 2 $ trToDeleteKeyboardBtn <$> t
        pure $ WithKeyboard (EditMessage msg msgTxt) kb

stopTrackRequestCallback :: Effects m => CallbackQuery -> [Text] -> m ()
stopTrackRequestCallback CallbackQuery {..} params = do
  if length params /= 3
    then logError [qms|wrong number of parameters ({length params}) for command stopTracking|]
    else do
      runTelegramM executeBotAction $ do
        let uid = readDecimal $ params !! 1
            trId = params !! 2
        lift . void $ DB.deleteTrackRequest uid (readDecimal trId)
        msg@Message {..} <- dieOnNothing cbkMessage NoAction
        let oldKb = inlineKeyboard $ fromMaybe (InlineKeyboardMarkup []) replyMarkup
            newKb = filter priceBtn oldKb
        pure $ WithKeyboard (EditMessageReplyMarkup msg) (InlineKeyboardMarkup newKb)
  where
    priceBtn [] = False
    priceBtn ((InlineKeyboardButton "stop tracking" _ _) : _) = False
    priceBtn _ = True

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

readDecimal :: Text -> Integer
readDecimal = T.foldl' (\a c -> a * 10 + toInteger (digitToInt c)) 0

trToListElem :: (Monoid a, IsString a) => DB.TrackRequest -> a
trToListElem DB.TrackRequest {..} = [qms|<a href="{trPodUrl}">{trName}</a>\n|]

trToDeleteKeyboardBtn :: DB.TrackRequest -> InlineKeyboardButton
trToDeleteKeyboardBtn DB.TrackRequest {..} =
  InlineKeyboardButton
    do [qms|delete: {trName}|]
    do Nothing
    do Just [qms|deleteTr,{trUserId},{fromMaybe (-1) trRequestId}|]

trToStopKeyboardBtn :: DB.TrackRequest -> InlineKeyboardButton
trToStopKeyboardBtn DB.TrackRequest {..} =
  InlineKeyboardButton
    do [qms|delete: {trName}|]
    do Nothing
    do Just [qms|stopTracking,{trUserId},{fromMaybe (-1) trRequestId}|]

pcToListElem :: (Monoid a, IsString a) => DB.PriceCheck -> a
pcToListElem DB.PriceCheck {..} = [qms|<a href="{pcUrl}">{pcName}</a>\n|]

pcToDeleteKeyboardBtn :: DB.PriceCheck -> InlineKeyboardButton
pcToDeleteKeyboardBtn DB.PriceCheck {..} =
  InlineKeyboardButton
    do [qms|delete:{pcName}|]
    do Nothing
    do Just [qms|deletePc,{pcUserId},{pcName}|]
