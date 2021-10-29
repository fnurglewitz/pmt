{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tracker.Tracker where

import App.Config
import App.Database (TrackRequest (searchQuery))
import qualified App.Database as DB
import App.Logging
import App.Monad
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import PoD.Api (doSearch)
import PoD.Parser ( hitToSearch, searchToUrl )
import PoD.Rendering (renderHit)
import PoD.Types (Hit (..), SearchResponse (SearchResponse, _hits))
import System.Random
import System.Random.Stateful
import System.Random.Stateful (globalStdGen)
import Telegram.Bot.Api.Client
import Telegram.Bot.Api.Types
import Telegram.Bot.Api.Types (SendPhotoRequest (SendPhotoRequest))

startTracker :: AppCtx Connection -> IO ()
startTracker ctx@AppCtx {..} = do
  logGeneric logger "INFO" config "Starting tracker"
  forever $ do
    r <- runExceptT $ runReaderT (runAppM runTracker) ctx
    case r of
      Left err -> logGeneric logger "ERROR" config err
      Right x -> pure x
    p <- randomPause
    threadDelay p

runTracker :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => m ()
runTracker = do
  AppCtx {..} <- ask
  mbt <- DB.findOneToTrack
  case mbt of
    Nothing -> return () -- TODO: reset flags
    Just t@DB.TrackRequest {..} -> do
      SearchResponse {..} <- liftIO $ doSearch searchQuery
      newTrades <- DB.findNewHits _hits
      sequence_ (notify t <$> newTrades)
      sequence_ (DB.saveTrade <$> newTrades)
      DB.setAsTracked userId (fromMaybe 0 requestId)

notify :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError Text m, TelegramClient m, DB.DB m, HasLogger m) => TrackRequest -> Hit -> m ()
notify tr@DB.TrackRequest {..} hit@Hit {..} = do
  ctx@(AppCtx (Config _ _ _ _ _ _ RenderConfig {..}) _ _ _ font) <- ask
  let image = renderHit font (fromIntegral rnDpi) hit
      url = searchToUrl . hitToSearch $ hit
      rid = T.pack . show $ fromMaybe 0 requestId
  sendPhoto $ SendPhotoRequest userId image $ Just (InlineKeyboardMarkup [[InlineKeyboardButton _note (Just url) Nothing], [InlineKeyboardButton "stop tracking" Nothing (Just (T.append "stopTracking," rid) )]])

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
