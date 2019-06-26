{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Run (runServer) where

import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (pack)
import Database.MongoDB.Connection (PortID(PortNumber))
import Database.Persist.MongoDB (MongoConf(..), defaultMongoConf, master, withConnection)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, Settings, Port, setPort, setLogger, defaultSettings)
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (Handler(..), serve, hoistServer)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Text.URI (parseURI, URI(..))

import API (API)
import Monads (Env(..), AppM)
import Server (handler)

env :: String -> String -> IO String
env varName def = fromMaybe def <$> lookupEnv varName

appMtoHandler :: forall a. Env -> AppM a -> Handler a
appMtoHandler e = Handler . ExceptT . flip runReaderT e . runExceptT

getApplication :: Env -> Application
getApplication e = simpleCors $ serve api $ hoistServer api (appMtoHandler e) handler
    where api = Proxy @API

parseUrl :: String -> Maybe MongoConf
parseUrl = parseURI >=> go
    where go (URI (Just "mongodb") _ (Just host) (Just port) ('/':dbname) _ _) = Just $ (defaultMongoConf $ pack dbname)
                { mgHost = pack host, mgPort = PortNumber (fromInteger port), mgAccessMode = master }
          go _ = Nothing

readPort :: String -> Int
readPort str = go $ readMaybe str
    where go (Just x) = x
          go Nothing = error $ "Expected port number to be an integer, got '" <> str <> "'"

warpSettings :: Port -> ApacheLogger -> Settings
warpSettings port logger = setLogger logger $ setPort port $ defaultSettings

runServer :: IO ()
runServer = withStdoutLogger $ \logger -> do
    url <- env "MONGODB_URL" "mongodb://mongo:27017/entrance-app"
    isDevelopment <- (== "development") <$> env "NODE_ENV" "development"
    port <- readPort <$> env "PORT" "8000"
    let mongoConf = fromMaybe
            (error $ "Malformed mongodb url, expected something like 'mongodb://<host>:<port>/<dbname>', got '" <> url <> "'")
            (parseUrl url)
    withConnection mongoConf (\pool -> runSettings (warpSettings port logger) $ getApplication $ Env { pool, isDevelopment })
