{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PoD.Parser (parsePodUri, hitToSearch, searchToUrl) where

import Control.Monad (join)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Read as TR
import GHC.Generics (Generic)
import Graphics.Rasterific.Linear (Metric (quadrance))
import Lens.Micro.Platform (ix, over, set, view, (^.), (^?))
import Network.URI.Encode (decodeText)
import PoD.Types
  ( Hit (..),
    ItemProperty (..),
    Property (..),
    SearchFilter (..),
    SearchFilterComponent (..),
    SearchQuery (..),
    corruptedItem,
    emptyProperty,
    etherealItem,
    item,
    itemJson,
    itemType,
    levelReq,
    levelReqComparitor,
    makeProperty,
    makeSearchQuery,
    makeValuedProperty,
    maxSockets,
    minSockets,
    nQuality,
    nQualityCode,
    need,
    poster,
    properties,
    quality,
    searchFilter,
    uncorruptedItem,
    unetherealItem,
  )
import Text.Parsec
  ( char,
    digit,
    many1,
    noneOf,
    oneOf,
    optional,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec.Error
  ( errorMessages,
    messageString,
  )
import Utils.Aeson (removeFieldLabelPrefix)

parseMultiText :: String -> ([Text] -> SearchFilterComponent) -> Parser SearchFilterComponent
parseMultiText label constructor = do
  optional (char '&')
  string label
  items <- T.pack <$$> many1 element
  return $ constructor items
  where
    (<$$>) = fmap . fmap
    element = optional (char ',') >> many1 (noneOf ['&', ','])

parseSingleText :: String -> (Text -> SearchFilterComponent) -> Parser SearchFilterComponent
parseSingleText label constructor = do
  optional (char '&')
  string label
  item <- T.pack <$> many1 (noneOf ['&'])
  return $ constructor item

parseInt :: String -> (Int -> SearchFilterComponent) -> Parser SearchFilterComponent
parseInt label constructor = do
  optional (char '&')
  string label
  num <- T.pack <$> many1 digit
  case TR.decimal num of
    Right n -> return $ (constructor . fst) n
    Left _ -> error "parseInt: should not be here"

parseEnglishBool :: String -> (Bool -> SearchFilterComponent) -> Parser SearchFilterComponent
parseEnglishBool label constructor = do
  optional (char '&')
  string label
  constructor . convert <$> (string "yes" <|> string "no")
  where
    convert "yes" = True
    convert "no" = False

selectedItemsParser :: Parser SearchFilterComponent
selectedItemsParser = parseMultiText "selectedItem=" SelectedItems

qualityParser :: Parser SearchFilterComponent
qualityParser = parseMultiText "quality=" Quality

selectedItemTypesParser :: Parser SearchFilterComponent
selectedItemTypesParser = parseSingleText "selectedItemType=" SelectedItemType

posterParser :: Parser SearchFilterComponent
posterParser = parseSingleText "poster=" Poster

corruptedParser :: Parser SearchFilterComponent
corruptedParser = parseEnglishBool "corrupted=" Corrupted

etherealParser :: Parser SearchFilterComponent
etherealParser = parseEnglishBool "ethereal=" Ethereal

levelReqParser :: Parser SearchFilterComponent
levelReqParser = parseSingleText "levelReq=" LevelReq

levelReqOpParser :: Parser SearchFilterComponent
levelReqOpParser = parseSingleText "levelReqOp=" LevelReqOp

minSocketsParser :: Parser SearchFilterComponent
minSocketsParser = parseSingleText "minSockets=" MinSockets

maxSocketsParser :: Parser SearchFilterComponent
maxSocketsParser = parseSingleText "maxSockets=" MaxSockets

wantParser :: Parser SearchFilterComponent
wantParser = parseSingleText "want=" Want

onlineOnlyParser :: Parser SearchFilterComponent
onlineOnlyParser = parseInt "onlineOnly=" OnlineOnly

propertiesParser :: Parser SearchFilterComponent
propertiesParser = parseSingleText "properties=" Properties

parsePodUriFragment :: Parser SearchFilterComponent
parsePodUriFragment =
  try selectedItemsParser
    <|> try qualityParser
    <|> try selectedItemTypesParser
    <|> try posterParser
    <|> try corruptedParser
    <|> try etherealParser
    <|> try levelReqParser
    <|> try levelReqOpParser
    <|> try minSocketsParser
    <|> try maxSocketsParser
    <|> try wantParser
    <|> try onlineOnlyParser
    <|> try propertiesParser

decodeProperties :: Text -> [Property]
decodeProperties t = do
  let bs = encodeUtf8 t
  case B64.decode bs >>= eitherDecodeStrict of
    Left err -> [] -- TODO
    Right props -> props

removeEmptyProperty :: [Property] -> [Property]
removeEmptyProperty = filter (/= emptyProperty)

pp = flip (++)

applySearchFilterComponent :: SearchQuery -> SearchFilterComponent -> SearchQuery
applySearchFilterComponent q (SelectedItems items) = over (searchFilter . item) (const items) q
applySearchFilterComponent q (Want want) = over (searchFilter . need) (const want) q
applySearchFilterComponent q (Quality qlty) = over (searchFilter . quality) (const qlty) q
applySearchFilterComponent q (Poster pstr) = over (searchFilter . poster) (const pstr) q
applySearchFilterComponent q (OnlineOnly _) = q -- won't use this tracker with a logged account
applySearchFilterComponent q (LevelReq lvlRq) = over (searchFilter . levelReq) (const lvlRq) q
applySearchFilterComponent q (LevelReqOp lvlRqCmp) = over (searchFilter . levelReqComparitor) (const lvlRqCmp) q
applySearchFilterComponent q (SelectedItemType sit) = over (searchFilter . itemType) (const . Just $ sit) q
applySearchFilterComponent q (Corrupted True) = over (searchFilter . properties) (pp [corruptedItem]) q
applySearchFilterComponent q (Corrupted False) = over (searchFilter . properties) (pp [uncorruptedItem]) q
applySearchFilterComponent q (Ethereal True) = over (searchFilter . properties) (pp [etherealItem]) q
applySearchFilterComponent q (Ethereal False) = over (searchFilter . properties) (pp [unetherealItem]) q
applySearchFilterComponent q (MinSockets min) = over (searchFilter . properties) (pp [minSockets min]) q
applySearchFilterComponent q (MaxSockets max) = over (searchFilter . properties) (pp [maxSockets max]) q
applySearchFilterComponent q (Properties props) = over (searchFilter . properties) (f props) q
  where
    f toAdd props = pp (decodeProperties toAdd) (removeEmptyProperty props)

podUriParser :: Parser SearchQuery
podUriParser = do
  string "https://beta.pathofdiablo.com/trade-search?"
  convert <$> many1 parsePodUriFragment
  where
    convert = foldr (flip applySearchFilterComponent) makeSearchQuery

parsePodUri :: Text -> Either Text SearchQuery
parsePodUri t = case parse podUriParser "PoD URI" normalized of
  Right x -> Right x
  Left err -> Left $ f err
  where
    -- TODO this shit sucks
    normalized = decodeText $ T.replace "+" " " t
    joinStr lst = Prelude.foldr (\x y -> x ++ "\n" ++ y) (Prelude.head lst) (Prelude.tail lst)
    f err = T.pack . joinStr $ messageString <$> errorMessages err

setProps :: [Property] -> SearchQuery -> SearchQuery
setProps = set (searchFilter . properties)

emptyQuery =
  set (searchFilter . quality) [] $
    setProps [] makeSearchQuery

-- uncorruptedItem, unetherealItem

itemPropToPartialSQ :: ItemProperty -> SearchQuery
itemPropToPartialSQ (ItemProperty "Corrupted" _ _ _) = setProps [corruptedItem] emptyQuery
itemPropToPartialSQ (ItemProperty "Socketed" (Just v) _ _) = setProps [minSockets v, maxSockets v] emptyQuery
itemPropToPartialSQ (ItemProperty "EtherealCannotbeRepaired" _ _ _) = setProps [etherealItem] emptyQuery
itemPropToPartialSQ (ItemProperty pCode Nothing _ _) = setProps [makeProperty pCode] emptyQuery
itemPropToPartialSQ (ItemProperty pCode (Just v) _ _) = setProps [makeValuedProperty pCode v] emptyQuery

hitToSearch :: Hit -> SearchQuery
hitToSearch h@Hit {..} =
  foldr1 (<>) $
    ( set (searchFilter . item) [_name] $
        set (searchFilter . quality) [normalizeQualityFilter $ h ^. itemJson . nQualityCode] $
          set (searchFilter . poster) _username $
            set
              (searchFilter . levelReq)
              (T.pack . show $ _lvlReq)
              emptyQuery
    ) :
    propz
  where
    propz = itemPropToPartialSQ <$> _itemProperties
    normalizeQualityFilter :: Text -> Text
    normalizeQualityFilter "Unique" = "Unique"
    normalizeQualityFilter "q_unique" = "Unique"
    normalizeQualityFilter "Set" = "Set"
    normalizeQualityFilter "q_set" = "Set"
    normalizeQualityFilter "Rare" = "Rare"
    normalizeQualityFilter "q_rare" = "Rare"
    normalizeQualityFilter "Magic" = "Magic"
    normalizeQualityFilter "q_magic" = "Magic"
    normalizeQualityFilter "Superior" = "Superior"
    normalizeQualityFilter "q_high" = "Superior"
    normalizeQualityFilter "Normal" = "Normal"
    normalizeQualityFilter "q_normal" = "Normal"
    normalizeQualityFilter "Craft" = "Crafted"
    normalizeQualityFilter "Crafted" = "Crafted"
    normalizeQualityFilter "q_crafted" = "Crafted"
    normalizeQualityFilter "Runeword" = "Runeword"
    normalizeQualityFilter "q_runeword" = "Runeword"
    normalizeQualityFilter f = f

searchToUrl :: SearchQuery -> Text
searchToUrl q@(SearchQuery SearchFilter {..}) =
  T.replace " " "+" $
    "https://beta.pathofdiablo.com/trade-search?"
      <> selectedItem
      <> quality
      <> poster
      <> itemType
      <> lvlReq
      <> corrupted
      <> ethereal
      <> minSockets
      <> maxSockets
      <> encodedProperties
  where
    wrap k = maybe "" (T.append k)
    wrap' c k = maybe (T.append k "no") (const (T.append k "yes")) (filter (\p -> Just c == _propertyCode p) _properties ^? ix 0)
    sockets qp pc cmp = maybe "" (T.append qp) (_propertyValue =<< filter (\p -> (Just pc == _propertyCode p) && (cmp == _comparitor p)) _properties ^? ix 0)

    selectedItem = wrap "selectedItem=" (_item ^? ix 0)
    quality = wrap "&quality=" (_quality ^? ix 0)
    poster = wrap "&poster=" (Just _poster)
    -- want = wrap "want=" (Just _need)
    itemType = wrap "&selectedItemType=" _itemType
    lvlReq = wrap "&levelReq=" (Just _levelReq)
    corrupted = wrap' "Corrupted" "&corrupted="
    ethereal = wrap' "EtherealCannotBeRepaired" "&ethereal="
    minSockets = sockets "&minSockets=" "Socketed" "gte"
    maxSockets = sockets "&maxSockets=" "Socketed" "lte"
    filteredProperties = filter (\p -> (Just "Corrupted" /= _propertyCode p) && (Just "EtherealCannotBeRepaired" /= _propertyCode p) && (Just "Socketed" /= _propertyCode p)) _properties
    encodedProperties = T.append "&properties=" $ decodeUtf8 $ B64.encode $ B.toStrict $ encode filteredProperties

{-
https://beta.pathofdiablo.com/trade-search?
    selectedItem=Arachnid+Mesh+Spiderweb+Sash
    &quality=Rare
    &selectedItemType=Amulet
    &poster=fnurglewitz
    &corrupted=yes
    &ethereal=no
    &levelReq=13
    &minSockets=1
    &maxSockets=3
    &want=cippalippa

&properties=W3sicHJvcGVydHlDb2RlIjoiRGVmZW5zZSIsImNvbXBhcml0b3IiOiIqIn1d
[{"propertyCode":"Defense","comparitor":"*"}]
-}

-- TODO: custom search string
{-
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
-}

parseLabelText :: String -> (Text -> SearchFilterComponent) -> Parser SearchFilterComponent
parseLabelText label constructor = do
  optional (many1 (char ' '))
  string label
  out <- T.pack <$> many1 (noneOf [' '])
  return $ constructor out

parseLabelDigitText :: String -> (Text -> SearchFilterComponent) -> Parser SearchFilterComponent
parseLabelDigitText label constructor = do
  optional (many1 (char ' '))
  string label
  out <- T.pack . c <$> oneOf "123456"
  return $ constructor out
  where
    c = flip (:) []

parseLabelPlusOptionsText :: String -> [String] -> (Text -> SearchFilterComponent) -> Parser SearchFilterComponent
parseLabelPlusOptionsText label options constructor = do
  optional (many1 (char ' '))
  string label
  out <- T.pack <$> f parsers
  return $ constructor out
  where
    parsers = fmap (try . string) options
    f (x : xs) = foldr (<|>) x xs

parseLabelPlusOptionsTextList :: String -> [String] -> ([Text] -> SearchFilterComponent) -> Parser SearchFilterComponent
parseLabelPlusOptionsTextList label options constructor = do
  optional (many1 (char ' '))
  string label
  out <- T.pack <$$> many1 (optional (char ',') >> f parsers)
  return $ constructor out
  where
    (<$$>) = fmap . fmap
    parsers = fmap (try . string) options
    f (x : xs) = foldr (<|>) x xs

parseLabelBool :: String -> (Bool -> SearchFilterComponent) -> Parser SearchFilterComponent
parseLabelBool label constructor = do
  res <- try (string $ "+" ++ label) <|> try (string $ "-" ++ label)
  return $ constructor (f res)
  where
    f (x : _)
      | x == '+' = True
      | otherwise = False

sItemNameParser :: Parser SearchFilterComponent
sItemNameParser = do
  optional (many1 (char ' '))
  char '"'
  itemName <- T.pack <$> many1 (noneOf ['"'])
  char '"'
  return $ SelectedItems [itemName]

sItemQualityParser :: Parser SearchFilterComponent
sItemQualityParser = parseLabelPlusOptionsTextList "q:" ["Unique", "Set", "Rare", "Magic", "Superior", "Normal", "Crafted", "Runeword"] Quality

sItemTypeParser :: Parser SearchFilterComponent
sItemTypeParser = parseLabelPlusOptionsText "t:" ["Amulet", "Assassin Weapon", "Barbarian Helm", "Belt", "Body Armor", "Boots", "Bow", "Circlet", "Crossbow", "Custom", "Druid Pelt", "Gloves", "Helm", "Keyset", "Multiple Runes", "Necromancer Shrunken Head", "One-handed Axe", "One-handed Sword", "Orb", "Paladin Shield", "Polearm", "Ring", "Rune", "Shield", "Spear", "Staff", "Throwing Weapon", "Two-handed Axe", "Two-handed Sword", "Wand"] SelectedItemType

sPosterParser :: Parser SearchFilterComponent
sPosterParser = parseLabelText "p:" Poster

sCorruptedParser :: Parser SearchFilterComponent
sCorruptedParser = parseLabelBool "cor" Corrupted

sEtherealParser :: Parser SearchFilterComponent
sEtherealParser = parseLabelBool "eth" Corrupted

sLevelReqParser :: Parser SearchFilterComponent
sLevelReqParser = undefined

sMinSocketsParser :: Parser SearchFilterComponent
sMinSocketsParser = parseLabelDigitText "minos:" MinSockets

sMaxSocketsParser :: Parser SearchFilterComponent
sMaxSocketsParser = parseLabelDigitText "maxos:" MaxSockets

parseSearchFragment :: Parser SearchFilterComponent
parseSearchFragment =
  try sItemQualityParser
    <|> try sItemTypeParser
    <|> try sPosterParser
    <|> try sCorruptedParser
    <|> try sEtherealParser
    <|> try sMinSocketsParser
    <|> try sMaxSocketsParser

searchQueryParser :: Parser SearchQuery
searchQueryParser = do
  itemName <- sItemNameParser
  convert . (:) itemName <$> many1 parseSearchFragment
  where
    convert = foldr (flip applySearchFilterComponent) makeSearchQuery

parseSearchQuery :: Text -> Either Text SearchQuery
parseSearchQuery t = case parse searchQueryParser "PoD Search Query" t of
  Right x -> Right x
  Left err -> Left $ f err
  where
    -- TODO this shit sucks
    joinStr lst = Prelude.foldr (\x y -> x ++ "\n" ++ y) (Prelude.head lst) (Prelude.tail lst)
    f err = T.pack . joinStr $ messageString <$> errorMessages err
