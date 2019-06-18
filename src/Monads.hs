{-# LANGUAGE UndecidableInstances #-}

module Monads (Env(..), AppM, Server, MonadDatabase(..)) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask)
import Database.Persist.MongoDB (Action, ConnectionPool, runMongoDBPool, master)
import Servant.Server (ServerT, ServantErr)

data Env = Env
    { pool :: ConnectionPool
    , isDevelopment :: Bool
    }

type AppM = ExceptT ServantErr (ReaderT Env IO)

type Server api = ServerT api AppM

class MonadIO m => MonadDatabase m where
    runQuery :: Action IO a -> m a

instance MonadDatabase AppM where
    runQuery x = do
        Env{ pool } <- ask
        liftIO $ runMongoDBPool master x pool
        --TODO: Maybe catch database errors
