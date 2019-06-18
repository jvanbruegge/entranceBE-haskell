module Database where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Class (insert)
import Database.Persist.Types (Entity(..))

import HelperModels (CreateMeeting(..))
import Models (DbMeeting(..), Meeting(..))
import Monads (MonadDatabase(runQuery))

createMeeting :: MonadDatabase m => Text -> CreateMeeting -> m Meeting
createMeeting office CreateMeeting{ host, phone, meeting, date } = do
    currentDate <- liftIO $ getCurrentTime
    let newMeeting = DbMeeting office host phone meeting date currentDate currentDate
    meetingId <- runQuery $ insert newMeeting
    pure $ MkMeeting $ Entity meetingId newMeeting
