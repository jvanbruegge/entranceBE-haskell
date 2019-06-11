{-# LANGUAGE DeriveAnyClass #-}

module HelperModels (CreateMeeting(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data CreateMeeting = CreateMeeting
    { host, phone, meeting :: Text
    , date :: UTCTime
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
