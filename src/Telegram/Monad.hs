{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Telegram.Monad where

import Control.Monad.Except ( MonadError (throwError) )
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.Text (Text)
import Streaming
  ( MonadIO (..)
  , MonadTrans (lift)
  , Of
  , Stream
  )
import qualified Streaming.Prelude as S
import Telegram.Bot.Api.Types ( Message (..), InlineKeyboardMarkup(..) )

data TelegramActionSimple = 
    SendMessage Integer Text
  | ReplyMessage Message Text
  | EditMessage Message Text
  | SendPhoto Integer B.ByteString
  | ReplyPhoto Message B.ByteString
  deriving (Show)

data TelegramAction = WithKeyboard TelegramActionSimple InlineKeyboardMarkup | NoKeyboard TelegramActionSimple | NoAction

newtype TelegramM m a = TelegramM { unTelegramM :: Stream (Of TelegramAction) (ExceptT TelegramAction m) a }
  deriving (Functor, Applicative, Monad, MonadError TelegramAction, MonadIO)

instance MonadTrans TelegramM where
  lift = TelegramM . lift . lift

-- the output of the handler is the success message
runTelegramM :: Monad m => (TelegramAction -> m ()) -> TelegramM m TelegramAction -> m ()
runTelegramM f (unTelegramM -> handler) = do
  action <- runExceptT $ S.mapM_
    do lift . f
    do handler
  either f f action

replyLog :: Monad m => TelegramAction -> TelegramM m ()
replyLog = TelegramM . S.yield

dieWith :: Monad m => TelegramAction -> TelegramM m b
dieWith = throwError

dieIf :: Monad m => Bool -> TelegramAction -> TelegramM m ()
dieIf True action = dieWith action
dieIf False _ = pure ()

dieOnNothing :: Monad m => Maybe b -> TelegramAction -> TelegramM m b
dieOnNothing Nothing t = dieWith t
dieOnNothing (Just x) _ = pure x

dieOnLeft :: Monad m => Either t b -> (t -> TelegramAction) -> TelegramM m b
dieOnLeft (Left e) f = dieWith $ f e
dieOnLeft (Right x) _ = pure x