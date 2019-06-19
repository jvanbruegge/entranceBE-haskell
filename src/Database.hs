module Database (
    createMeeting, getMeetings, deleteMeeting, deleteAllMeetings, getMeetingsToday, getUpcomingMeetings
) where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Persist ((==.), (<=.), (>=.))
import Database.Persist.Class (insert, selectList, delete, deleteWhere)
import Database.Persist.Types (Entity(..), SelectOpt(Asc), Filter)

import HelperModels (CreateMeeting(..))
import Models (DbMeeting(..), Meeting(..), MeetingId, EntityField(DbMeetingDate, DbMeetingOffice))
import Monads (MonadDatabase(runQuery), MonadTime(getCurrentTime))
import Helpers (endOfToday, startOfToday)

createMeeting :: (MonadDatabase m, MonadTime m) => Text -> CreateMeeting -> m Meeting
createMeeting office CreateMeeting{ host, phone, meeting, date } = do
    currentDate <- getCurrentTime
    let newMeeting = DbMeeting office host phone meeting date currentDate currentDate
    meetingId <- runQuery $ insert newMeeting
    pure $ MkMeeting $ Entity meetingId newMeeting

getMeetings :: MonadDatabase m => Text -> m [Meeting]
getMeetings = listMeetings []

deleteMeeting :: MonadDatabase m => MeetingId -> m ()
deleteMeeting = runQuery . delete

deleteAllMeetings :: MonadDatabase m => Text -> m ()
deleteAllMeetings office = runQuery $ deleteWhere [DbMeetingOffice ==. office]

getMeetingsToday :: (MonadDatabase m, MonadTime m) => Text -> m [Meeting]
getMeetingsToday = getMeetingsFromToday JustToday

getUpcomingMeetings :: (MonadDatabase m, MonadTime m) => Text -> m [Meeting]
getUpcomingMeetings = getMeetingsFromToday AlsoInTheFuture

data InTheFuture = JustToday | AlsoInTheFuture
    deriving (Show, Eq)

getMeetingsFromToday :: (MonadDatabase m, MonadTime m) => InTheFuture -> Text -> m [Meeting]
getMeetingsFromToday future office = do
    start <- startOfToday
    end <- endOfToday
    listMeetings ((DbMeetingDate >=. start):[DbMeetingDate <=. end | future == JustToday]) office

listMeetings :: MonadDatabase m => [Filter DbMeeting] -> Text -> m [Meeting]
listMeetings filters office = fmap coerce $ runQuery $ selectList ((DbMeetingOffice ==. office):filters) [Asc DbMeetingDate]
