module API where

import Data.Text (Text)
import Servant

import HelperModels (CreateMeeting)
import Models (Meeting, MeetingId)

type API = OfficeAPI
         :<|> "health" :> Get '[PlainText, JSON] NoContent

type OfficeAPI = "meetings" :> MeetingAPI --TODO: Remove after clients have migrated
                :<|> Capture "office" Text :> "meetings" :> MeetingAPI

type MeetingAPI = ReqBody '[JSON] CreateMeeting :> Post '[JSON] Meeting
                :<|> Get '[JSON] [Meeting]
                :<|> "q" :> QueryAPI
                :<|> Capture "id" MeetingId :> Delete '[PlainText, JSON] NoContent
                :<|> Delete '[PlainText, JSON] NoContent --TODO: Delete after dev finished

type QueryAPI = "today" :> Get '[JSON] [Meeting]
              :<|> "upcoming" :> Get '[JSON] [Meeting]
