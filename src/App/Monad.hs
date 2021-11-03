{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module App.Monad where

import App.Config (AppCtx)
import Control.Monad.Except ( MonadError, throwError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Control.Monad.Trans.Except ( runExceptT, ExceptT )
import Streaming
  ( MonadTrans (lift)
  , Of
  , Stream
  )
import qualified Streaming.Prelude as S

newtype AppM x e m a = AppM {runAppM :: ReaderT (AppCtx x) (ExceptT e m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppCtx x), MonadError e)

newtype ReplyM s e m a = ReplyM { unReplyM :: Stream (Of s) (ExceptT e m) a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadIO)

instance MonadTrans (ReplyM s e) where
  lift = ReplyM . lift . lift

-- the output of the handler is the success message
runReplyM :: Monad m => (s -> m a) -> (e-> m a) -> ReplyM s e m a -> m a
runReplyM f g (unReplyM -> handler) = do
  action <- runExceptT $ S.mapM_
    do lift . f
    do handler
  either g pure action

replyLog :: Monad m => s -> ReplyM s e m ()
replyLog = ReplyM . S.yield

dieWith :: Monad m => e -> ReplyM s e m a
dieWith = throwError

dieIf :: Monad m => Bool -> e -> ReplyM s e m ()
dieIf True action = dieWith action
dieIf False _ = pure ()

dieOnNothing :: Monad m => Maybe a -> e -> ReplyM s e m a
dieOnNothing Nothing t = dieWith t
dieOnNothing (Just x) _ = pure x

dieOnLeft :: Monad m => Either t a -> (t -> e) -> ReplyM s e m a
dieOnLeft (Left e) f = dieWith $ f e
dieOnLeft (Right x) _ = pure x