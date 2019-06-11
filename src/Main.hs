module Main where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

env :: String -> String -> IO String
env varName def = fromMaybe def <$> lookupEnv varName

main :: IO ()
main = do
    url <- env "MONGODB_URL" "mongodb://mongo:27017/entrance-app"
    development <- (== "development") <$> env "NODE_ENV" "development"
    port <- fromMaybe (error "Port number has to be an integer value") . readMaybe @Int <$> env "PORT" "8000"
    print port
