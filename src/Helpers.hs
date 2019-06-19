module Helpers (endOfToday) where

import Data.Fixed (Fixed(..), resolution, E12)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (zonedTimeToLocalTime, zonedTimeToUTC, TimeOfDay(..), localTimeOfDay)

import Monads (MonadTime(getZonedTime))

endOfToday :: MonadTime m => m UTCTime
endOfToday = do
        now <- getZonedTime
        let end = (zonedTimeToLocalTime now) { localTimeOfDay = TimeOfDay 23 59 (MkFixed $ 60 * resolution (Proxy @E12)) }
        pure $ zonedTimeToUTC $ now { zonedTimeToLocalTime = end }
