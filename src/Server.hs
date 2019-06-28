module Server (handler) where

import Control.Monad ((<=<))
import Data.Text (Text)
import Servant (NoContent(..), (:<|>) (..))

import API (API, OfficeAPI, MeetingAPI, QueryAPI)
import Database (createMeeting, getMeetings, deleteMeeting, deleteAllMeetings, getMeetingsToday, getUpcomingMeetings)
import Monads (Server)

handler :: Server API
handler = officeHandler :<|> healthHandler
    where healthHandler = pure NoContent

officeHandler :: Server OfficeAPI
officeHandler = meetingHandler "munich" :<|> meetingHandler

meetingHandler :: Text -> Server MeetingAPI
meetingHandler office = createMeeting office
                      :<|> getMeetings office
                      :<|> queryHandler office
                      :<|> discardResult deleteMeeting
                      :<|> discardResult deleteAllMeetings office

queryHandler :: Text -> Server QueryAPI
queryHandler office = getMeetingsToday office :<|> getUpcomingMeetings office

discardResult :: Monad m => (a -> m b) -> a -> m NoContent
discardResult = (<=<) $ const $ pure NoContent
