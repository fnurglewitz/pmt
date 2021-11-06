{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module App.Database where

import App.Config (DatabaseConfig (..), db)
import App.Monad (AppM)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO, asks, liftIO)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
  ( ConnectInfo (ConnectInfo)
  , Connection
  , FromRow
  , In (In)
  , Only (fromOnly)
  , ToRow
  , connect
  , execute
  , execute_
  , executeMany
  , query
  , query_
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import PoD.Types (Hit (..), SearchQuery)

getConnection :: DatabaseConfig -> IO Connection
getConnection DatabaseConfig {..} = connect $ ConnectInfo (T.unpack pgHost) 5432 (T.unpack pgUser) (T.unpack pgPass) (T.unpack pgDb)

class (Monad m) => DB m where
  saveAuth :: Auth -> m ()
  getAuth :: Integer -> m (Maybe Auth)

  saveTrackRequest :: TrackRequest -> m ()
  listTrackRequest :: Integer -> m [TrackRequest]
  deleteTrackRequest :: Integer -> Integer -> m ()
  findOneToTrack :: m (Maybe TrackRequest)
  setAsTracked :: Integer -> Integer -> UTCTime -> m ()
  resetTrackRequests :: m ()

  saveHit :: Hit -> m ()
  findHit :: Text -> m (Maybe Hit)
  findNewHitsForRequest :: Integer -> [Hit] -> m [Hit]
  saveHitsForRequest :: Integer -> [Hit] -> m ()

  savePc :: PriceCheck -> m ()
  findPc :: Integer -> Text -> m (Maybe PriceCheck)
  listPc :: Integer -> m [PriceCheck]
  deletePc :: Integer -> Text -> m ()
  configurePc :: Integer -> Text -> Maybe Text -> m ()

instance (MonadIO m) => DB (AppM Connection e m) where
  saveAuth Auth {..} = do
    conn <- asks db
    void $
      liftIO $
        execute
          conn
          [sql|INSERT INTO public.auth (user_id, request_ts, enabled, max_price_checks, max_track_requests, username) VALUES (?, ?, ?, ?, ?, ?)|]
          (aUserId, aRequestTs, aEnabled, aMaxPriceChecks, aMaxTrackRequests, aUsername)

  getAuth uid = do
    conn <- asks db
    r <- liftIO $ query conn "select user_id, request_ts, enabled, max_price_checks, max_track_requests, username from public.auth where user_id = ? limit 1" [uid]
    return $ listToMaybe r

  saveTrackRequest TrackRequest {..} = do
    conn <- asks db
    void $
      liftIO $
        execute
          conn
          [sql| INSERT INTO public.track_request (user_id, name, pod_url, tracked, search_query, last_track_ts, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)|]
          (trUserId, trName, trPodUrl, trTracked, trSearchQuery, trLastTrackTs, trCreatedAt)

  listTrackRequest userId = do
    conn <- asks db
    liftIO $ query conn "select request_id, user_id, name, pod_url, tracked, search_query, last_track_ts, created_at from public.track_request where user_id = ?" [userId]

  deleteTrackRequest userId requestId = do
    conn <- asks db
    void $
      liftIO $
        execute
          conn
          "delete from public.track_request where user_id = ? and request_id = ?"
          (userId, requestId)

  findOneToTrack = do
    conn <- asks db
    r <- liftIO $ query_ conn "select request_id, user_id, name, pod_url, tracked, search_query, last_track_ts, created_at from public.track_request where tracked = false limit 1"
    return $ listToMaybe r

  setAsTracked uid id' time = do
    conn <- asks db
    void $ liftIO $ execute conn "update public.track_request set tracked = true, last_track_ts = ? where user_id = ? and request_id = ?" (time, uid, id')

  resetTrackRequests = do
    conn <- asks db
    void $ liftIO $ execute_ conn "update public.track_request set tracked = false"

  saveHit hit@Hit {..} = do
    conn <- asks db
    now <- liftIO getCurrentTime
    void $
      liftIO $
        execute
          conn
          [sql|INSERT INTO public.hit (trade_id, note, hit, created_at) VALUES (?, ?, ?, ?) ON CONFLICT (trade_id, note) DO NOTHING;|]
          (_tradeId, _note, hit, now)

  findHit tid = do
    conn <- asks db
    hit <- liftIO $ query conn "select hit from public.hit where trade_id = ? order by created_at desc limit 1" [tid]
    return $ listToMaybe (fromOnly <$> hit)

  findNewHitsForRequest rid hits = do
    conn <- asks db
    existing <- liftIO $ query conn [sql|
          select B.hit
          from
            public.track_request_hit A
              inner join public.hit B on A.trade_id = B.trade_id and A.note = B.note
          where 
                  A.request_id = ? 
              and A.trade_id in ?
          |] (rid, In $ _tradeId <$> hits)
    return $ findNew (fromOnly <$> existing)
    where
      findNew t = S.toList $ S.fromList hits S.\\ S.fromList t

  saveHitsForRequest rid hits = do
    conn <- asks db
    void $
      liftIO $
        executeMany
          conn
          [sql| INSERT INTO public.track_request_hit (request_id, trade_id, note) VALUES (?, ?, ?)|] (hitToRow <$> hits)
    where
      hitToRow Hit{..} = (rid, _tradeId, _note)

  savePc PriceCheck {..} = do
    conn <- asks db
    now <- liftIO getCurrentTime
    void $
      liftIO $
        execute
          conn
          [sql| INSERT INTO public.price_check (user_id, name, url, search_query, created_at) VALUES (?, ?, ?, ?, ?)|]
          (pcUserId, pcName, pcUrl, pcSearchQuery, now)

  findPc uid n = do
    conn <- asks db
    liftIO $ query conn "select pc_id, user_id, name, url, search_query, config, created_at from public.price_check where user_id = ? and name = ?" (uid, n) <&> listToMaybe

  listPc uid = do
    conn <- asks db
    liftIO $ query conn "select pc_id, user_id, name, url, search_query, config from public.price_check where user_id = ?" [uid]

  deletePc uid name = do
    conn <- asks db
    void $ liftIO $ execute conn "delete from public.price_check where user_id = ? and name = ?" (uid, name)

  configurePc uid name config = do
    conn <- asks db
    void $ liftIO $ execute conn "update public.price_check set config = ? where user_id = ? and name = ?" (config, uid, name)

data TrackRequest = TrackRequest
  { trRequestId :: Maybe Integer
  , trUserId :: Integer
  , trName :: Text
  , trPodUrl :: Text
  , trTracked :: Bool
  , trSearchQuery :: SearchQuery
  , trLastTrackTs :: UTCTime
  , trCreatedAt :: UTCTime
  }
  deriving (Generic, FromRow, ToRow, Show)

data PriceCheck = PriceCheck
  { pcId :: Maybe Integer
  , pcUserId :: Integer
  , pcName :: Text
  , pcUrl :: Text
  , pcSearchQuery :: SearchQuery
  , pcConfig :: Maybe Text
  , pcCreatedAt :: UTCTime
  }
  deriving (Generic, FromRow, ToRow, Show)

data Auth = Auth
  { aUserId :: Integer
  , aRequestTs :: UTCTime
  , aEnabled :: Bool
  , aMaxPriceChecks :: Integer
  , aMaxTrackRequests :: Integer
  , aUsername :: Text
  }
  deriving (Generic, FromRow, ToRow, Show)
