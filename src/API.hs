module API where

import Data.Text (Text)
import Servant

import Models (Meeting, MeetingId)
import HelperModels (CreateMeeting)

type API = OfficeAPI
         :<|> "health" :> Get '[] NoContent

type OfficeAPI = Capture "office" Text :> "meetings" :> MeetingAPI
               :<|> "meetings" :> MeetingAPI --TODO: Remove after clients have migrated

type MeetingAPI = Post '[JSON] CreateMeeting
                :<|> Get '[JSON] [Meeting]
                :<|> "q" :> QueryAPI
                :<|> Capture "id" MeetingId :> Delete '[] NoContent
                :<|> Delete '[] NoContent --TODO: Delete after dev finished

type QueryAPI = "today" :> Get '[JSON] [Meeting]
              :<|> "upcoming" :> Get '[JSON] [Meeting]
