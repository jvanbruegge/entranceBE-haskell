{-# LANGUAGE TemplateHaskell #-}

module Tests.HelperTests (helperTests) where

import Control.Monad.Reader (MonadReader, Reader, ask, runReader)
import Data.Proxy (Proxy(..))
import Data.Fixed (Fixed(..), resolution, E12)
import Data.Time.Clock (UTCTime, diffUTCTime, NominalDiffTime, secondsToNominalDiffTime)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.LocalTime (ZonedTime(..), zonedTimeToUTC, utcToZonedTime, TimeZone(..), LocalTime(..), TimeOfDay(..))
import Hedgehog (discover, checkSequential, Property, Gen, property, forAll, assert, annotateShow)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Monads (MonadTime(..))
import qualified Helpers as SUT

helperTests :: IO Bool
helperTests = checkSequential $$(discover)

oneDay :: Integer
oneDay = 24 * 60 * 60

prop_startOfDay_max24hourDiff :: Property
prop_startOfDay_max24hourDiff = mkProperty $ \_ end time -> end `diffUTCTime` time < fromInteger oneDay

prop_startOfDay_diff_endOfDay_is24hours :: Property
prop_startOfDay_diff_endOfDay_is24hours = mkProperty $ \start end _ -> end `diffUTCTime` start == fromInteger oneDay `minusPico` 1

prop_start_time_end_sameDay :: Property
prop_start_time_end_sameDay = mkProperty $ \start end time ->
        let ZonedTime (LocalTime startDay _) _ = utcToZonedTime cestTimeZone start
            ZonedTime (LocalTime endDay _) _ = utcToZonedTime cestTimeZone end
            ZonedTime (LocalTime day _) _ = utcToZonedTime cestTimeZone time
        in day == startDay && day == endDay

------------------------------ Helper Stuff -------------------------------------
newtype PureTime a = MkPureTime { unPureTime :: Reader ZonedTime a }
    deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader ZonedTime PureTime

instance MonadTime PureTime where
    getCurrentTime = zonedTimeToUTC <$> ask
    getZonedTime = ask

minusPico :: NominalDiffTime -> Integer -> NominalDiffTime
minusPico time n = time - diffTime
    where diffTime = secondsToNominalDiffTime $ MkFixed n

cestTimeZone :: TimeZone
cestTimeZone = TimeZone 120 True "CEST"

mkProperty :: (UTCTime -> UTCTime -> UTCTime -> Bool) -> Property
mkProperty f = property $ do
    time <- forAll genZonedTime

    let start = runReader (unPureTime SUT.startOfToday) time
        end = runReader (unPureTime SUT.endOfToday) time

    annotateShow $ utcToZonedTime cestTimeZone start
    annotateShow $ utcToZonedTime cestTimeZone end

    assert $ f start end $ zonedTimeToUTC time

genZonedTime :: Gen ZonedTime
genZonedTime = do
    day <- Gen.integral (Range.linear 1858 3000)
    hours <- Gen.int (Range.linear 0 23)
    minutes <- Gen.int (Range.linear 0 59)
    seconds <- Gen.integral (Range.linear 0 $ 60 * resolution (Proxy @E12) - 1)
    let localTime = LocalTime (ModifiedJulianDay day) $ TimeOfDay hours minutes $ MkFixed seconds
    pure $ ZonedTime localTime cestTimeZone
