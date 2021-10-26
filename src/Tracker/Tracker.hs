{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tracker.Tracker where

import App.Config

import Control.Concurrent (threadDelay)
import Control.Monad
import Database.PostgreSQL.Simple


runTracker :: AppCtx Connection -> IO ()
runTracker ctx@AppCtx{..} = forever $ do
  threadDelay 1000000

