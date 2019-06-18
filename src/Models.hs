{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Models (
    User(..), Meeting(..), DbMeeting(..), MeetingId, UserId
) where

import Data.Aeson (ToJSON(..), Value(Object))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.TH (share, mkPersist, persistLowerCase)
import qualified Data.HashMap.Strict as Map

import MongoSettings (mongoSettings)

share
    [mkPersist mongoSettings]
    [persistLowerCase|
DbUser json
    name Text
    phone Text
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show Eq
|]

share
    [mkPersist mongoSettings]
    [persistLowerCase|
DbMeeting json sql=meetings
    office Text
    host Text
    phone Text
    meeting Text
    date UTCTime
    createdAt UTCTime sql=createdAt
    updatedAt UTCTime sql=updatedAt
    deriving Show Eq
|]

type UserId = DbUserId
newtype User = MkUser (Entity DbUser)
    deriving ToJSON via (WithId DbUser)

type MeetingId = DbMeetingId
newtype Meeting = MkMeeting (Entity DbMeeting)
    deriving ToJSON via (WithId DbMeeting)

newtype WithId a = MkWithId (Entity a)
instance (ToJSON a, ToJSON (Key a)) => ToJSON (WithId a) where
    toJSON (MkWithId (Entity key object)) = Object $ Map.insert "_id" (toJSON key) (getValues $ toJSON object)
        where getValues (Object hashmap) = hashmap
              getValues _ = error "Can only wrap objects with WithId"
