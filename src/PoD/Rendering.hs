{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PoD.Rendering (renderHit) where

import Codec.Picture (PixelRGBA8 (..), PngSavable (encodePng))
import Control.Monad (foldM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor (($>))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Rasterific
  ( Drawing,
    PointSize (PointSize),
    V2 (V2),
    printTextAt,
    renderDrawingAtDpi,
    withTexture,
  )
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Text.TrueType
  ( BoundingBox (..),
    Dpi,
    Font,
    stringBoundingBox,
  )
import Lens.Micro.Platform ((<&>), (^.))
import PoD.Types
  ( Hit (..),
    nDamageMaximum,
    nDamageMinimum,
    nDefense,
    nDurability,
    nDurabilityMaximum,
    nItemLevel,
    nLevelReq,
    nPropertyList,
    nQualityCode,
    nTag,
    nTitle,
  )

data TextBox = TextBox
  { bFont :: Font,
    bPointSize :: PointSize,
    bColor :: PixelRGBA8,
    bText :: Text,
    bWidth :: Float,
    bHeight :: Float
  }

headerSize, propsSize :: PointSize
headerSize = PointSize 14.0
propsSize = PointSize 10.0

blue, white, yellow, orange, gold, red, green, normal, eth, black, runeword :: PixelRGBA8
blue = PixelRGBA8 98 123 251 255
white = PixelRGBA8 255 255 255 255
yellow = PixelRGBA8 252 232 115 255
orange = PixelRGBA8 255 140 0 255
gold = PixelRGBA8 207 185 124 255
red = PixelRGBA8 150 0 0 255
green = PixelRGBA8 93 198 0 255
normal = PixelRGBA8 210 210 210 255
eth = PixelRGBA8 128 128 128 255
black = PixelRGBA8 0 0 0 255
runeword = PixelRGBA8 158 135 73 255

qualityColor :: Text -> PixelRGBA8 -- TODO: use nQualityCode ?
qualityColor "Unique" = gold
qualityColor "q_unique" = gold
qualityColor "Set" = green
qualityColor "q_set" = green
qualityColor "Rare" = yellow
qualityColor "q_rare" = yellow
qualityColor "Magic" = blue
qualityColor "q_magic" = blue
qualityColor "Superior" = eth
qualityColor "q_high" = eth
qualityColor "Normal" = normal
qualityColor "q_normal" = normal
qualityColor "Craft" = orange
qualityColor "Crafted" = orange
qualityColor "q_crafted" = orange
qualityColor "Runeword" = runeword
qualityColor "q_runeword" = runeword
qualityColor x = error $ "qualityColor not known: " <> T.unpack x


boxHeight, boxWidth :: BoundingBox -> Float
boxHeight BoundingBox {..} = abs _yMin + abs _yMax
boxWidth BoundingBox {..} = abs _xMin + abs _xMax

makeBoundingBox :: Font -> Dpi -> PointSize -> Text -> BoundingBox
makeBoundingBox font dpi pointSize text = stringBoundingBox font dpi pointSize (T.unpack text)

makeTextBox :: Font -> Dpi -> PointSize -> PixelRGBA8 -> Text -> Maybe TextBox
makeTextBox f d s c t = Just $ TextBox f s c t (boxWidth $ makeBoundingBox f d s t) (boxHeight $ makeBoundingBox f d s t)

toTextBoxes :: Font -> Dpi -> Hit -> [TextBox]
toTextBoxes font dpi Hit {..} =
  let qColor = qualityColor $ _itemJson ^. nQualityCode
      itemName = makeTextBox font dpi headerSize qColor $ T.toUpper $ _itemJson ^. nTitle
      itemType = makeTextBox font dpi headerSize qColor $ T.toUpper $ _itemJson ^. nTag
      defense = _itemJson ^. nDefense <&> T.toUpper >>= \def -> makeTextBox font dpi propsSize white $ T.append "DEFENSE: " def
      damage = do
        min' <- _itemJson ^. nDamageMinimum
        max' <- _itemJson ^. nDamageMaximum
        let str = foldr T.append "" ["DAMAGE: ", min', "-", max']
        makeTextBox font dpi propsSize white str
      durability = do
        min' <- _itemJson ^. nDurability
        max' <- _itemJson ^. nDurabilityMaximum
        let str = foldr T.append "" ["DURABILITY: ", min', " OF ", max']
        makeTextBox font dpi propsSize white str
      itemLvl = makeTextBox font dpi propsSize white $ T.append "ITEM LEVEL: " (_itemJson ^. nItemLevel)
      levelReq = makeTextBox font dpi propsSize white $ T.append "REQUIRED LEVEL: " (T.pack $ show $ _itemJson ^. nLevelReq)
   in fmap fromJust $ filter isJust [itemName, itemType, defense, damage, durability, itemLvl, levelReq] ++ propList
  where
    propertiesToTextBoxes :: Font -> Dpi -> [Text] -> [Maybe TextBox]
    propertiesToTextBoxes font' dpi' lines' = f <$> lines'
      where
        f txt
          | "Corrupted" `T.isSuffixOf` txt = makeTextBox font' dpi' propsSize red "CORRUPTED"
          | otherwise = makeTextBox font' dpi propsSize blue txt'
          where
            txt' = T.toUpper txt
    propList = propertiesToTextBoxes font dpi (_itemJson ^. nPropertyList)

-- offset from top/side and between lines
pixelOffset :: Float
pixelOffset = 5.0

imgWidth :: [TextBox] -> Int
imgWidth b = round $ pixelOffset + foldr max 0.0 (bWidth <$> b)

imgHeight :: [TextBox] -> Int
imgHeight b = round $ pixelOffset + sum (pixelOffset <$ b) + sum (bHeight <$> b)

drawLine :: Float -> Float -> TextBox -> Drawing PixelRGBA8 Float
drawLine xMax oldY TextBox {..} = drawText $> y
  where
    y = oldY + pixelOffset + bHeight
    x = (xMax - bWidth) / 2
    drawText = withTexture (uniformTexture bColor) $ printTextAt bFont bPointSize (V2 x y) (T.unpack bText)

renderImage :: Font -> Int -> [TextBox] -> B.ByteString
renderImage _font dpi boxes =
  BL.toStrict $
    encodePng $
      renderDrawingAtDpi w h dpi black $ do
        foldM_ drawLine' 0.0 boxes
  where
    w = imgWidth boxes
    h = imgHeight boxes
    drawLine' = drawLine (fromIntegral w)

renderHit :: Font -> Int -> Hit -> B.ByteString
renderHit font dpi hit = renderImage font dpi (toTextBoxes font dpi hit)

{-
renderImage' :: Font -> Int -> Hit -> B.ByteString
renderImage' font dpi hit =
  BL.toStrict $
    encodePng $
      renderDrawingAtDpi w h dpi black $ do
        foldM_ drawLine' 0.0 boxes
  where
    boxes = toTextBoxes font dpi hit
    w = imgWidth boxes
    h = imgHeight boxes
    drawLine' = drawLine (fromIntegral w)
-}
