{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Monad (AppM (..)) where

import App.Config (AppCtx)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)

newtype AppM x e m a = AppM {runAppM :: ReaderT (AppCtx x) (ExceptT e m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppCtx x), MonadError e)
