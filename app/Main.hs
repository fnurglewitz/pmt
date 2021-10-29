{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import App.Database
import App.Config
import App.Logging

import Tracker.Tracker

import Telegram.Bot.Bot

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Text.TrueType (BoundingBox (..), Dpi, Font (..), loadFontFile, pointInPixelAtDpi, stringBoundingBox) -- TODO: clean
import Options.Applicative

main :: IO ()
main = do
  cfg@(Config{..}) <- customExecParser p cfgParserInfo
  font <- loadFont (rnFontPath renderCfg)
  conn <- getConnection databaseCfg
  loggerSet <- getLoggerSet
  luid <- newMVar (0 :: Integer)
  let ctx = AppCtx cfg loggerSet conn luid font
  forkIO $ startTracker ctx
  print cfg
  startBot ctx
  where
    p = prefs $ showHelpOnEmpty <> showHelpOnError

loadFont :: Text -> IO Font
loadFont path = do 
  mbFont <- loadFontFile (T.unpack path)
  case mbFont of
    Left err -> error err 
    Right font -> return font