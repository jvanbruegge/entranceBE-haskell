{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Models (
    User(..), Meeting(..), UserId, MeetingId
) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.TH (share, mkPersist, persistLowerCase)

import MongoSettings (mongoSettings)

share
    [mkPersist mongoSettings]
    [persistLowerCase|
User json
    name Text
    phone Text
    createdAt UTCTime default=new Date()
    updatedAt UTCTime default=new Date()
    deriving Show Eq
|]

share
    [mkPersist mongoSettings]
    [persistLowerCase|
Meeting json
    office Text
    host Text
    phone Text
    meeting Text
    date UTCTime
    createdAt UTCTime default=new Date()
    updatedAt UTCTime default=new Date()
    deriving Show Eq
|]
