{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PoD.Types
  ( SearchFilterComponent (..)
  , SearchQuery (..)
  , SearchFilter (..)
  , Property (..)
  , SearchResponse (..)
  , Hit (..)
  , ItemProperty (..)
  , ItemJson (..)
  , Position (..)
  , Size (..)
  , makeSearchQuery
  , corruptedItem
  , uncorruptedItem
  , etherealItem
  , unetherealItem
  , minSockets
  , maxSockets
  , emptyProperty
  , makeProperty
  , makeValuedProperty
  , searchFilter
  , item
  , need
  , quality
  , poster
  , levelReq
  , levelReqComparitor
  , itemType
  , properties
  , propertyCode
  , comparitor
  , propertyValue
  , hits
  , name
  , tradeId
  , itemProperties
  , createdAt
  , userId
  , note
  , username
  , iQuality
  , iType
  , qualityCode
  , characterName
  , difficulty
  , itemJson
  , lvlReq
  , lastInGame
  , lastOnline
  , prf
  , itemPropertyCode
  , itemPropertyValue
  , itemPropertyLabel
  , itemPropertyOriginalText
  , nQuality
  , nId
  , nQualityCode
  , nTitle
  , nTag
  , nTextTag
  , nEthereal
  , nEnhanced
  , nDamageMinimum
  , nDamageMaximum
  , nDefense
  , nDurability
  , nDurabilityMaximum
  , nAlternateGraphics
  , nItemLevel
  , nLevelReq
  , nSocketCount
  , nPropertyList
  , nPosition
  , nSize
  , nWorn
  , nName
  , nTradeId
  , x
  , y
  , invheight
  , invwidth
  , gameMode
  , onlineOnly
  )
where

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (..)
  , encode
  , genericParseJSON
  , genericToJSON
  , object
  , withEmbeddedJSON
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField (toField), toJSONField)
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeLenses)
import Utils.Aeson (removeFieldLabelPrefix)

data SearchFilterComponent
  = SelectedItems [Text]
  | Quality [Text]
  | SelectedItemType Text
  | Poster Text
  | Corrupted Bool
  | Ethereal Bool
  | LevelReq Text
  | LevelReqOp Text
  | MinSockets Text
  | MaxSockets Text
  | Want Text
  | OnlineOnly Int
  | Properties Text
  deriving (Eq, Show)

newtype SearchQuery = SearchQuery
  { _searchFilter :: SearchFilter
  }
  deriving (Eq, Show, Generic)

instance ToJSON SearchQuery where
  toJSON = genericToJSON (removeFieldLabelPrefix False "_")

instance FromJSON SearchQuery where
  parseJSON = genericParseJSON (removeFieldLabelPrefix False "_")

instance Semigroup SearchQuery where
  (<>) (SearchQuery f) (SearchQuery g) = SearchQuery (f <> g)

data SearchFilter = SearchFilter
  { _item :: [Text]
  , _need :: Text
  , _quality :: [Text]
  , _gameMode :: Text
  , _poster :: Text
  , _onlineOnly :: Bool
  , _levelReq :: Text
  , _levelReqComparitor :: Text
  , _properties :: [Property]
  , _itemType :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON SearchFilter where
  toJSON = genericToJSON (removeFieldLabelPrefix False "_")

instance FromJSON SearchFilter where
  parseJSON = genericParseJSON (removeFieldLabelPrefix False "_")

instance Semigroup SearchFilter where
  (<>) (SearchFilter i n q g ps o lr lrc p it) (SearchFilter i' n' q' _g' ps' o' lr' _lrc' p' it') =
    SearchFilter (i <> i') (n <> n') (q <> q') g (ps <> ps') (o || o') (lr <> lr') lrc (p <> p') (it <> it')

data Property = Property
  { _propertyCode :: Maybe Text
  , _comparitor :: Text
  , _propertyValue :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Property where
  toJSON = genericToJSON (removeFieldLabelPrefix False "_")

instance FromJSON Property where
  parseJSON = withObject "Property" $ \v ->
    Property
      <$> v .:? "propertyCode"
      <*> v .: "comparitor"
      <*> v .:? "propertyValue"

newtype SearchResponse = SearchResponse
  { _hits :: [Hit]
  }
  deriving (Eq, Show, Generic)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \v ->
    SearchResponse
      <$> v .: "hits"

data Hit = Hit
  { _name :: Text
  , _tradeId :: Text
  , _itemProperties :: [ItemProperty]
  , _createdAt :: UTCTime
  , _userId :: Integer
  , _note :: Text
  , _username :: Text
  , _iQuality :: Text
  , _iType :: Text
  , _qualityCode :: Text
  , _characterName :: Text
  , _difficulty :: Text
  , _itemJson :: ItemJson
  , _lvlReq :: Integer
  , _lastInGame :: Text
  , _lastOnline :: Text
  , _prf :: Text
  }
  deriving (Show, Generic)

instance Eq Hit where
  (==) h1 h2 = _tradeId h1 == _tradeId h2 && _note h1 == _note h2

instance Ord Hit where
  compare h1 h2 = _tradeId h1 `compare` _tradeId h2 <> _note h1 `compare` _note h2

instance FromJSON Hit where
  parseJSON = withObject "Hit" $ \v ->
    Hit
      <$> v .: "name"
      <*> v .: "trade_id"
      <*> v .: "item_properties"
      <*> v .: "created_at"
      <*> v .: "user_id"
      <*> v .: "note"
      <*> v .: "username"
      <*> v .: "quality"
      <*> v .: "itemType"
      <*> v .: "qualityCode"
      <*> v .: "characterName"
      <*> v .: "difficulty"
      <*> (v .: "item_json" >>= \x -> withEmbeddedJSON "ItemJson" parseJSON (String x))
      <*> v .: "levelReq"
      <*> v .: "lastInGame"
      <*> v .: "lastOnline"
      <*> v .: "prf"

instance ToJSON Hit where
  toJSON Hit {..} =
    object
      [ "name" .= _name
      , "trade_id" .= _tradeId
      , "item_properties" .= _itemProperties
      , "created_at" .= _createdAt
      , "user_id" .= _userId
      , "note" .= _note
      , "username" .= _username
      , "quality" .= _iQuality
      , "itemType" .= _iType
      , "qualityCode" .= _qualityCode
      , "characterName" .= _characterName
      , "difficulty" .= _difficulty
      , -- "item_json" .= _itemJson,
        "item_json" .= (decodeUtf8 . B.toStrict . encode $ _itemJson)
      , "levelReq" .= _lvlReq
      , "lastInGame" .= _lastInGame
      , "lastOnline" .= _lastOnline
      , "prf" .= _prf
      ]

data ItemProperty = ItemProperty
  { _itemPropertyCode :: Text
  , _itemPropertyValue :: Maybe Text
  , _itemPropertyLabel :: Text
  , _itemPropertyOriginalText :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ItemProperty where
  parseJSON = withObject "ItemProperty" $ \v ->
    ItemProperty
      <$> v .: "propertyCode"
      <*> v .:? "propertyValue"
      <*> v .: "label"
      <*> v .: "originalText"

instance ToJSON ItemProperty where
  toJSON ItemProperty {..} =
    object
      [ "propertyCode" .= _itemPropertyCode
      , "propertyValue" .= _itemPropertyValue
      , "label" .= _itemPropertyLabel
      , "originalText" .= _itemPropertyOriginalText
      ]

data ItemJson = ItemJson
  { _nQuality :: Text
  , _nId :: Text
  , _nQualityCode :: Text
  , _nTitle :: Text
  , _nTag :: Text
  , _nTextTag :: Text
  , _nEthereal :: Text
  , _nEnhanced :: Text
  , _nDamageMinimum :: Maybe Text
  , _nDamageMaximum :: Maybe Text
  , _nDefense :: Maybe Text
  , _nDurability :: Maybe Text
  , _nDurabilityMaximum :: Maybe Text
  , _nAlternateGraphics :: Maybe Text
  , _nItemLevel :: Text
  , _nLevelReq :: Integer
  , _nSocketCount :: Text
  , _nPropertyList :: [Text]
  , _nPosition :: Position
  , _nSize :: Size
  , _nWorn :: Text
  , _nName :: Text
  , _nTradeId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ItemJson where
  parseJSON = withObject "ItemJson" $ \v ->
    ItemJson
      <$> v .: "Quality"
      <*> v .: "ID"
      <*> v .: "QualityCode"
      <*> v .: "Title"
      <*> v .: "Tag"
      <*> v .: "TextTag"
      <*> v .: "Ethereal"
      <*> v .: "Enhanced"
      <*> v .:? "DamageMinimum"
      <*> v .:? "DamageMaximum"
      <*> v .:? "Defense"
      <*> v .:? "Durability"
      <*> v .:? "DurabilityMaximum"
      <*> v .:? "AlternateGraphics"
      <*> v .: "ItemLevel"
      <*> v .: "LevelReq"
      <*> v .: "SocketCount"
      <*> v .: "PropertyList"
      <*> v .: "Position"
      <*> v .: "Size"
      <*> v .: "Worn"
      <*> v .: "Name"
      <*> v .: "trade_id"

instance ToJSON ItemJson where
  toJSON ItemJson {..} =
    object
      [ "Quality" .= _nQuality
      , "ID" .= _nId
      , "QualityCode" .= _nQualityCode
      , "Title" .= _nTitle
      , "Tag" .= _nTag
      , "TextTag" .= _nTextTag
      , "Ethereal" .= _nEthereal
      , "Enhanced" .= _nEnhanced
      , "DamageMinimum" .= _nDamageMinimum
      , "DamageMaximum" .= _nDamageMaximum
      , "Defense" .= _nDefense
      , "Durability" .= _nDurability
      , "DurabilityMaximum" .= _nDurabilityMaximum
      , "AlternateGraphics" .= _nAlternateGraphics
      , "ItemLevel" .= _nItemLevel
      , "LevelReq" .= _nLevelReq
      , "SocketCount" .= _nSocketCount
      , "PropertyList" .= _nPropertyList
      , "Position" .= _nPosition
      , "Size" .= _nSize
      , "Worn" .= _nWorn
      , "Name" .= _nName
      , "trade_id" .= _nTradeId
      ]

data Position = Position
  { _x :: Integer
  , _y :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Position where
  parseJSON = withObject "Position" $ \v ->
    Position
      <$> v .: "x"
      <*> v .: "y"

instance ToJSON Position where
  toJSON = genericToJSON (removeFieldLabelPrefix False "_")

data Size = Size
  { _invheight :: Integer
  , _invwidth :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Size where
  parseJSON = withObject "Size" $ \v ->
    Size
      <$> v .: "invheight"
      <*> v .: "invwidth"

instance ToJSON Size where
  toJSON = genericToJSON (removeFieldLabelPrefix False "_")

$(makeLenses ''SearchQuery)
$(makeLenses ''SearchFilter)
$(makeLenses ''Property)

$(makeLenses ''SearchResponse)
$(makeLenses ''Hit)
$(makeLenses ''ItemProperty)
$(makeLenses ''ItemJson)
$(makeLenses ''Position)
$(makeLenses ''Size)

makeSearchQuery :: SearchQuery
makeSearchQuery =
  SearchQuery $
    SearchFilter
      { _item = []
      , _need = ""
      , _quality = ["All"]
      , _gameMode = "softcore"
      , _poster = ""
      , _onlineOnly = False
      , _levelReq = ""
      , _levelReqComparitor = "="
      , _properties =
          [ Property
              { _propertyCode = Nothing
              , _comparitor = "*"
              , _propertyValue = Nothing
              }
          ]
      , _itemType = Nothing
      }

corruptedItem :: Property
corruptedItem =
  Property
    { _propertyCode = Just "Corrupted"
    , _comparitor = "*"
    , _propertyValue = Nothing
    }

uncorruptedItem :: Property
uncorruptedItem =
  Property
    { _propertyCode = Just "Corrupted"
    , _comparitor = "not"
    , _propertyValue = Nothing
    }

etherealItem :: Property
etherealItem =
  Property
    { _propertyCode = Just "EtherealCannotBeRepaired"
    , _comparitor = "*"
    , _propertyValue = Nothing
    }

unetherealItem :: Property
unetherealItem =
  Property
    { _propertyCode = Just "EtherealCannotBeRepaired"
    , _comparitor = "not"
    , _propertyValue = Nothing
    }

minSockets :: Text -> Property
minSockets x' =
  Property
    { _propertyCode = Just "Socketed"
    , _comparitor = "gte"
    , _propertyValue = Just x'
    }

maxSockets :: Text -> Property
maxSockets x' =
  Property
    { _propertyCode = Just "Socketed"
    , _comparitor = "lte"
    , _propertyValue = Just x'
    }

emptyProperty :: Property
emptyProperty =
  Property
    { _propertyCode = Nothing
    , _comparitor = "*"
    , _propertyValue = Nothing
    }

makeProperty :: Text -> Property
makeProperty c =
  Property
    { _propertyCode = Just c
    , _comparitor = "*"
    , _propertyValue = Nothing
    }

makeValuedProperty :: Text -> Text -> Property
makeValuedProperty c v =
  Property
    { _propertyCode = Just c
    , _comparitor = "="
    , _propertyValue = Just v
    }

instance FromField SearchQuery where
  fromField = fromJSONField

instance ToField SearchQuery where
  toField = toJSONField

instance FromField Hit where
  fromField = fromJSONField

instance ToField Hit where
  toField = toJSONField
