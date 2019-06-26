module Helpers (endOfToday, startOfToday) where

import Data.Fixed (Fixed(..), resolution, E12)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (zonedTimeToLocalTime, zonedTimeToUTC, TimeOfDay(..), localTimeOfDay)

import Monads (MonadTime(getZonedTime))

endOfToday :: MonadTime m => m UTCTime
endOfToday = jumpTime $ TimeOfDay 23 59 $ MkFixed $ 60 * resolution (Proxy @E12) - 1

startOfToday :: MonadTime m => m UTCTime
startOfToday = jumpTime $ TimeOfDay 0 0 $ MkFixed 0

jumpTime :: MonadTime m => TimeOfDay -> m UTCTime
jumpTime t = do
        now <- getZonedTime
        let end = (zonedTimeToLocalTime now) { localTimeOfDay = t }
        pure $ zonedTimeToUTC $ now { zonedTimeToLocalTime = end }

