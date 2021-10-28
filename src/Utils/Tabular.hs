module Utils.Tabular (priceCheckTable) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Tabular
  ( SemiTable,
    Table,
    col,
    empty,
    row,
    (+.+),
    (^..^),
    (^|^),
  )
import Text.Tabular.AsciiArt (render)

tupleToRow :: (Text, [Text]) -> SemiTable String String
tupleToRow (a, ts) = row (T.unpack a) (T.unpack <$> ts)

emptyPcTable :: Table rh String a
emptyPcTable = empty ^..^ col "cfg" [] ^|^ col "price" []

priceCheckTable :: [(Text, [Text])] -> Text
priceCheckTable [] = T.pack $ render id id id emptyPcTable
priceCheckTable xs = T.pack $ render id id id $ foldl (+.+) emptyPcTable (tupleToRow <$> xs)
