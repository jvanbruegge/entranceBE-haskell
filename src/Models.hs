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
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as Map

import MongoSettings (mongoSettings)

share
    [mkPersist mongoSettings]
    [persistLowerCase|
User json
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

newtype Meeting = MkMeeting (Entity DbMeeting)
type MeetingId = DbMeetingId

instance ToJSON Meeting where
    toJSON (MkMeeting (Entity key meeting)) = Object $ Map.insert "_id" (toJSON key) (getValues $ toJSON meeting)
        where getValues (Object hashmap) = hashmap
              getValues _ = error "Can not happen, meeting is always an object"
