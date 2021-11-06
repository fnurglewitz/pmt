{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Tracker.Tracker (startTracker) where

import App.Config
  ( AppCtx (..)
  , Config (Config)
  , RenderConfig (RenderConfig, rnDpi, rnFontPath)
  )
import App.Database (TrackRequest (trSearchQuery))
import qualified App.Database as DB
import App.Logging (HasLogger, logGeneric)
import App.Monad (AppM (runAppM))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad (forever)
import Control.Monad.Except (MonadError, MonadIO (..), runExceptT)
import Control.Monad.Reader
  ( MonadReader (ask)
  , ReaderT (runReaderT)
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import PoD.Api (doSearch)
import PoD.Parser (hitToSearch, searchToUrl)
import PoD.Rendering (renderHit)
import PoD.Types (Hit (..), SearchResponse (SearchResponse, _hits))
import System.Random.Stateful
  ( UniformRange (uniformRM)
  , globalStdGen
  )
import Telegram.Bot.Api.Client (TelegramClient (sendPhoto))
import Telegram.Bot.Api.Types
  ( InlineKeyboardButton (InlineKeyboardButton)
  , InlineKeyboardMarkup (InlineKeyboardMarkup)
  , SendPhotoRequest (SendPhotoRequest)
  )
import Text.InterpolatedString.QM (qms)

tshow :: Show a => a -> Text
tshow = T.pack . show

startTracker :: AppCtx Connection -> IO ()
startTracker ctx@AppCtx {..} = do
  logGeneric logger "INFO" config "Starting tracker"
  forever $ handle 
    do (\e -> logGeneric logger "ERROR" config $ tshow (e :: SomeException)) 
    do
      r <- runExceptT $ runReaderT (runAppM runTracker) ctx
      case r of
        Left err -> logGeneric logger "ERROR" config err
        Right x -> pure x
      randomPause >>= threadDelay

runTracker :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => m ()
runTracker = do
  mbt <- DB.findOneToTrack
  case mbt of
    Nothing -> DB.resetTrackRequests
    Just t@DB.TrackRequest {..} -> do
      let rid = fromMaybe 0 trRequestId
      SearchResponse {..} <- liftIO $ doSearch trSearchQuery
      mapM_ DB.saveHit _hits
      newHits <- DB.findNewHitsForRequest rid _hits
      sequence_ (notify t <$> newHits)
      DB.saveHitsForRequest rid newHits
      now <- liftIO getCurrentTime
      DB.setAsTracked trUserId rid now

notify :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => TrackRequest -> Hit -> m ()
notify DB.TrackRequest {..} hit@Hit {..} = do
  AppCtx (Config _ _ _ _ _ _ RenderConfig {..}) _ _ _ font <- ask
  let image = renderHit font (fromIntegral rnDpi) hit
      url = searchToUrl . hitToSearch $ hit
      rid = T.pack . show $ fromMaybe 0 trRequestId
      kb = Just (InlineKeyboardMarkup [[InlineKeyboardButton [qms|{_username}: {_note}|] (Just url) Nothing], [InlineKeyboardButton "stop tracking" Nothing (Just [qms|stopTracking,{trUserId},{rid}|])]])
  sendPhoto $ SendPhotoRequest trUserId image kb

randomPause :: IO Int
randomPause = do
  m <- randElem minutes
  s <- randRng =<< randElem seconds
  return $ (m * oneMinute) + (s * oneSecond)
  where
    oneSecond = 1000000
    oneMinute = 60000000
    minutes = [0, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 0]
    seconds = [(5, 15), (15, 30), (15, 30), (15, 30), (30, 45), (30, 45), (15, 30), (15, 30), (15, 30), (5, 15)]
    randRng r = uniformRM r globalStdGen
    randElem lst = (lst !!) <$> randRng (0, length lst - 1)
