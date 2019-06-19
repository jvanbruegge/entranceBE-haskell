{-# LANGUAGE UndecidableInstances #-}

module Monads (Env(..), AppM, Server, MonadDatabase(..), MonadTime(..)) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import Database.Persist.MongoDB (Action, ConnectionPool, runMongoDBPool, master)
import Servant.Server (ServerT, ServantErr)
import qualified Data.Time.Clock as T
import qualified Data.Time.LocalTime as T

data Env = Env
    { pool :: ConnectionPool
    , isDevelopment :: Bool
    }

type AppM = ExceptT ServantErr (ReaderT Env IO)

type Server api = ServerT api AppM

class Monad m => MonadDatabase m where
    runQuery :: Action IO a -> m a

class Monad m => MonadTime m where
    getCurrentTime :: m UTCTime
    getZonedTime :: m ZonedTime

instance MonadDatabase AppM where
    runQuery x = do
        Env{ pool } <- ask
        liftIO $ runMongoDBPool master x pool
        --TODO: Maybe catch database errors

instance MonadTime AppM where
    getCurrentTime = liftIO $ T.getCurrentTime
    getZonedTime = liftIO $ T.getZonedTime
