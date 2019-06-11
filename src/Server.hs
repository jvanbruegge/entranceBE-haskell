module Server (handler) where

import Data.Text (Text)
import Servant (NoContent(..), (:<|>) (..))
import Servant.Server (Server)

import API (API, OfficeAPI, MeetingAPI, QueryAPI)

handler :: Server API
handler = officeHandler :<|> healthHandler
    where healthHandler = pure NoContent

officeHandler :: Server OfficeAPI
officeHandler = meetingHandler :<|> meetingHandler "munich"

meetingHandler :: Text -> Server MeetingAPI
meetingHandler office = createMeeting office
                      :<|> getMeetings office
                      :<|> queryHandler office
                      :<|> deleteMeeting
                      :<|> deleteAllMeetings
    where createMeeting = undefined
          getMeetings = undefined
          deleteMeeting = undefined
          deleteAllMeetings = undefined

queryHandler :: Text -> Server QueryAPI
queryHandler office = getMeetingsToday office :<|> getUpcomingMeetings office
    where getMeetingsToday = undefined
          getUpcomingMeetings = undefined
