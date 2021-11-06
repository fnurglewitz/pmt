{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module App.Monad.AppM  where

import App.Config (AppCtx)
import Control.Monad.Except ( MonadError )
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Control.Monad.Trans.Except ( ExceptT )

newtype AppM x e m a = AppM {runAppM :: ReaderT (AppCtx x) (ExceptT e m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppCtx x), MonadError e)
