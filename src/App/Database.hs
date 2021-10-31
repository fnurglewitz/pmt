{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module App.Database where

import App.Config (AppCtx, DatabaseConfig (..), db)
import App.Monad
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, liftIO, runReaderT)
import Data.Aeson.Types (Value)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
  ( ToField (..),
    toJSONField,
  )
import GHC.Generics (Generic)
import PoD.Types

getConnection :: DatabaseConfig -> IO Connection
getConnection DatabaseConfig {..} = connect $ ConnectInfo (T.unpack pgHost) 5432 (T.unpack pgUser) (T.unpack pgPass) (T.unpack pgDb)

class (Monad m) => DB m where
  saveAuth :: Auth -> m ()
  getAuth :: Integer -> m (Maybe Auth)

  saveTrackRequest :: TrackRequest -> m ()
  listTrackRequest :: Text -> m [TrackRequest]
  deleteTrackRequest :: Text -> Integer -> m ()
  findOneToTrack :: m (Maybe TrackRequest)
  setAsTracked :: Text -> Integer -> UTCTime -> m ()
  resetTrackRequests :: m ()

  saveTrade :: Hit -> m ()
  findTrade :: Text -> m [TradeListing]
  findNewHits :: [Hit] -> m [Hit]

  savePc :: PriceCheck -> m ()
  findPc :: Text -> Text -> m (Maybe PriceCheck)
  listPc :: Text -> m [PriceCheck]
  deletePc :: Text -> Text -> m ()
  configurePc :: Text -> Text -> Maybe Text -> m ()

instance (MonadIO m) => DB (AppM Connection e m) where
  saveAuth Auth {..} = do
    conn <- asks db
    liftIO $
      execute
        conn
        [sql| INSERT INTO public.auth (user_id, request_time, enabled, max_price_checks, max_track_requests, username) 
                      VALUES (?, ?, ?, ?, ?, ?) |]
        (aUserId, aRequestTime, aEnabled, aMaxPriceChecks, aMaxTrackRequests, aUsername)
    return ()

  getAuth uid = do
    conn <- asks db
    r <- liftIO $ query conn "select user_id, request_time, enabled, max_price_checks, max_track_requests, username from public.auth where user_id = ? limit 1" [uid]
    return $ listToMaybe r

  saveTrackRequest req@TrackRequest {..} = do
    conn <- asks db
    liftIO $
      execute
        conn
        [sql| INSERT INTO public.track_request (user_id, pod_url, tracked, last_track_time, created_at, search_query, notes) 
                      VALUES (?, ?, ?, ?, ?, ?, ?)
                      |]
        (userId, podUrl, tracked, rLastTrackTime, rCreatedAt, searchQuery, notes)
    return ()

  listTrackRequest userId = do
    conn <- asks db
    liftIO $ query conn "select request_id, user_id, pod_url, search_query, notes, tracked, last_track_time, created_at from public.track_request where user_id = ?" [userId]

  deleteTrackRequest userId requestId = do
    conn <- asks db
    liftIO $
      execute
        conn
        "delete from public.track_request where user_id = ? and request_id = ?"
        (userId, requestId)
    return ()

  findOneToTrack = do
    conn <- asks db
    r <- liftIO $ query_ conn "select request_id, user_id, pod_url, search_query, notes, tracked, last_track_time, created_at from public.track_request where tracked = false limit 1"
    return $ listToMaybe r

  setAsTracked uid id time = do
    conn <- asks db
    liftIO $ execute conn "update public.track_request set tracked = true, last_track_time = ? where user_id = ? and request_id = ?" (time, uid, id)
    return ()

  resetTrackRequests = do
    conn <- asks db
    liftIO $ execute_ conn "update public.track_request set tracked = false"
    return ()

  saveTrade hit@Hit {..} = do
    conn <- asks db
    liftIO $
      execute
        conn
        [sql| INSERT INTO public.trade_listing (trade_id, seller_id, seller, note, difficulty, char_name, created_at, hit) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                      ON CONFLICT (trade_id) DO UPDATE
                        SET seller_id = ?,
                            seller = ?,
                            note = ?,
                            difficulty = ?,
                            char_name = ?,
                            created_at = ?,
                            hit = ?;
                      |]
        (_tradeId, _userId, _username, _note, _difficulty, _characterName, _createdAt, hit, _userId, _username, _note, _difficulty, _characterName, _createdAt, hit)
    return ()

  findTrade tid = do
    conn <- asks db
    liftIO $ query conn "select * from public.trade_listing where trade_id = ?" [tid]

  findNewHits hits = do
    conn <- asks db
    existing <- liftIO $ query conn "select hit from public.trade_listing where trade_id in ?" (Only (In $ _tradeId <$> hits)) -- TODO: doesn't work for multi-user...
    return $ findNew (fromOnly <$> existing)
    where
      findNew t = S.toList $ S.fromList hits S.\\ S.fromList t

  savePc pc@PriceCheck {..} = do
    conn <- asks db
    liftIO $
      execute
        conn
        [sql| INSERT INTO public.price_check (user_id, name, url, search_query) VALUES (?, ?, ?, ?)|]
        (pcUserId, pcName, pcUrl, pcSearchQuery)
    return ()

  findPc uid n = do
    conn <- asks db
    liftIO $ query conn "select pc_id, user_id, name, url, search_query, config from public.price_check where user_id = ? and name = ?" (uid, n) <&> listToMaybe

  listPc uid = do
    conn <- asks db
    liftIO $ query conn "select pc_id, user_id, name, url, search_query, config from public.price_check where user_id = ?" [uid]

  deletePc uid name = do
    conn <- asks db
    liftIO $ execute conn "delete from public.price_check where user_id = ? and name = ?" (uid, name)
    return ()

  configurePc uid name config = do
    conn <- asks db
    liftIO $ execute conn "update public.price_check set config = ? where user_id = ? and name = ?" (config, uid, name)
    return ()

data TradeListing = TradeListing
  { tradeId :: Text,
    sellerId :: Integer,
    seller :: Text,
    note :: Text,
    difficulty :: Text,
    charName :: Text,
    createdAt :: UTCTime,
    hit :: Hit
  }
  deriving (Generic, FromRow, ToRow, Show)

data TrackRequest = TrackRequest
  { requestId :: Maybe Integer,
    userId :: Text,
    podUrl :: Text,
    searchQuery :: SearchQuery,
    notes :: Text,
    tracked :: Bool,
    rLastTrackTime :: UTCTime,
    rCreatedAt :: UTCTime
  }
  deriving (Generic, FromRow, ToRow, Show)

instance FromField SearchQuery where
  fromField = fromJSONField

instance ToField SearchQuery where
  toField = toJSONField

instance FromField Hit where
  fromField = fromJSONField

instance ToField Hit where
  toField = toJSONField

data PriceCheck = PriceCheck
  { pcId :: Maybe Integer,
    pcUserId :: Text,
    pcName :: Text,
    pcUrl :: Text,
    pcSearchQuery :: SearchQuery,
    pcConfig :: Maybe Text
  }
  deriving (Generic, FromRow, ToRow, Show)

data Auth = Auth
  { aUserId :: Integer,
    aRequestTime :: UTCTime,
    aEnabled :: Bool,
    aMaxPriceChecks :: Integer,
    aMaxTrackRequests :: Integer,
    aUsername :: Text
  }
  deriving (Generic, FromRow, ToRow, Show)
