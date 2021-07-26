{-# LANGUAGE OverloadedStrings #-}

module Habit where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ToText (ToText (..))

data DayOfWeek
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Show)

instance ToText DayOfWeek where
  toText Sunday = "sun"
  toText Monday = "mon"
  toText Tuesday = "tue"
  toText Wednesday = "wed"
  toText Thursday = "thu"
  toText Friday = "fri"
  toText Saturday = "sat"

data Three
  = One
  | Two
  | Three
  deriving (Eq, Ord, Show)

instance ToText Three where
  toText One = "1"
  toText Two = "2"
  toText Three = "3"

data Frequency
  = Daily
  | Days (NESet DayOfWeek)
  | NoDays (NESet DayOfWeek)
  | Weekly Three
  | Monthly
  deriving (Eq, Show)

instance ToText Frequency where
  toText Daily = "daily"
  toText (Days days) = Text.unwords $ fmap toText $ NonEmpty.toList $ NESet.toList days
  toText (NoDays days) = "no " <> Text.unwords (fmap toText $ NonEmpty.toList $ NESet.toList days)
  toText (Weekly One) = "week"
  toText (Weekly n) = "week " <> toText n
  toText Monthly = "month"

type Name = Text

data Habit
  = Habit Name Frequency
  deriving (Eq, Show)

instance ToText Habit where
  toText (Habit name freq) = name <> " {" <> toText freq <> "}"
