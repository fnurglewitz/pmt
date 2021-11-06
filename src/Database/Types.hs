
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple ( FromRow, ToRow )
import GHC.Generics (Generic)
import PoD.Types (Hit, SearchQuery)

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
