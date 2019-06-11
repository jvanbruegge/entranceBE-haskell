module API where

import Servant

import Models (Meeting)
import HelperModels (CreateMeeting)

type API = OfficeAPI
         :<|> "health" :> Get '[] NoContent

type OfficeAPI = Capture "office" :> "meetings" :> MeetingAPI
               :<|> "meetings" :> MeetingAPI --TODO: Remove after clients have migrated

type MeetingAPI = Post '[JSON] CreateMeeting
                :<|> Get '[JSON] Meeting
                :<|> "q" :> QueryAPI
                :<|> Capture "id" :> Delete '[] NoContent
                :<|> Delete '[] NoContent --TODO: Delete after dev finished

type QueryAPI = "today" :> Get '[JSON] [Meeting]
              :<|> "upcoming" :> Get '[JSON] [Meeting]
