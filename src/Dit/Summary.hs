module Dit.Summary where

import Data.DateTime (DateTime)
import Dit.Event
import Dit.Habit

-- HERE:
-- Let's start showing what habits are pending for today
--
-- Daily
--   > find the latest Entry event and check if it was today
--
-- Days (Set DayOfWeek)
--   > find if today event exists and set includes today
--
-- NoDays (Set DayOfWeek)
--   > find if today event exists and set doest not include today
--
-- Weekly One
--   > find latest event and check if it was this week (today and past)
--
-- Weekly Two / Three
--   > find latest event and check if it was at most past 1 (or 2) weeks (today and past)
--
-- Monthly
--   > find latest event and check if it was during this month

isTodayPending :: DateTime -> Habit -> [Event] -> Bool
isTodayPending today (Habit _ Daily) events = True
isTodayPending _ _ _ = False

-- dayOfWeek <$> utctDay <$> getCurrentTime
-- > Monday
-- dayOfWeek <$> utctDay <$> (addUTCTime nominalDay) <$> getCurrentTime
-- > Tuesday

data Summary = Summary
  { sumAddedAt :: DateTime,
    sumLongestStreak :: Int,
    sumCurrentStreak :: Int,
    sumEventsAfterDone :: [Event],
    sumPendingToday :: Bool
  }
  deriving (Eq, Show)

summarize :: [Event] -> Either String Summary
summarize [] = Left "Empty (TODO: better message)"
summarize (Event Add _ addedAt : rest) = summarize' addedAt rest
summarize (Event event _ _ : _) =
  Left $ "First event should be Add, got " <> show event <> " instead"

summarize' :: DateTime -> [Event] -> Either String Summary
-- TODO: check if is pending today
summarize' addedAt [] = Right $ Summary addedAt 0 0 [] True
summarize' _ _ = undefined

-- TODO: keep the added date in the state ?
-- TODO: check current period
-- TODO: find the simplest version to start with (eg. pending today?)
--
-- HERE !!!!

-- summarize'' :: DateTime -> State -> [Event] -> Either String Summary
-- summarize'' addedAt st (Event Finish habit finishedAt : rest) =
--   Right $
--     Summary
--       { sumAddedAt = addedAt
--       }

data State = State
  { stFreq :: Frequency,
    stStreaks :: [Int],
    stCurrentStreak :: [DateTime],
    stStatus :: Status
  }
  deriving (Eq, Show)
