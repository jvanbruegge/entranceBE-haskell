module Database where

import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist ((==.))
import Database.Persist.Class (insert, selectList, delete, deleteWhere)
import Database.Persist.Types (Entity(..), SelectOpt(Asc))

import HelperModels (CreateMeeting(..))
import Models (DbMeeting(..), Meeting(..), MeetingId, EntityField(DbMeetingDate, DbMeetingOffice))
import Monads (MonadDatabase(runQuery))

createMeeting :: MonadDatabase m => Text -> CreateMeeting -> m Meeting
createMeeting office CreateMeeting{ host, phone, meeting, date } = do
    currentDate <- liftIO $ getCurrentTime
    let newMeeting = DbMeeting office host phone meeting date currentDate currentDate
    meetingId <- runQuery $ insert newMeeting
    pure $ MkMeeting $ Entity meetingId newMeeting

getMeetings :: MonadDatabase m => Text -> m [Meeting]
getMeetings office = fmap coerce $ runQuery $ selectList [DbMeetingOffice ==. office] [Asc DbMeetingDate]

deleteMeeting :: MonadDatabase m => MeetingId -> m ()
deleteMeeting = runQuery . delete

deleteAllMeetings :: MonadDatabase m => Text -> m ()
deleteAllMeetings office = runQuery $ deleteWhere [DbMeetingOffice ==. office]
