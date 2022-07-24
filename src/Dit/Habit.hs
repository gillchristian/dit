{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Dit.Habit where

import Control.Exception (SomeException (SomeException))
import Data.Functor (($>), (<&>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ToText (ToText (..))
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import qualified Text.Parsec as P
import Text.Parsec.Extra (checkWordEnd)
import Text.Parsec.Text (Parser)

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

dayOfWeekP :: Parser DayOfWeek
dayOfWeekP =
  P.choice
    [ P.try (P.string "sun") <* checkWordEnd $> Sunday,
      P.try (P.string "mon") <* checkWordEnd $> Monday,
      P.try (P.string "tue") <* checkWordEnd $> Tuesday,
      P.try (P.string "wed") <* checkWordEnd $> Wednesday,
      P.try (P.string "thu") <* checkWordEnd $> Thursday,
      P.try (P.string "fri") <* checkWordEnd $> Friday,
      P.string "sat" <* checkWordEnd $> Saturday
    ]

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

instance FromField Frequency where
  fromField :: FieldParser Frequency
  fromField fieldValue =
    case fieldData fieldValue of
      SQLText val -> case P.parse frequencyP "SQLText" val of
        Right freq -> Ok freq
        Left err -> Errors [SomeException $ Incompatible (show err) "Frequency" ""]
      other -> Errors [SomeException $ Incompatible (show other) "Status" ""]

instance ToField Frequency where
  toField :: Frequency -> SQLData
  toField = SQLText . toText

frequencyP :: Parser Frequency
frequencyP =
  P.choice
    [ P.try daily $> Daily,
      P.try weekly *> weeks <&> Weekly,
      P.try monthly $> Monthly,
      P.try (Days <$> days),
      NoDays <$> noDays
    ]
  where
    daily = P.string "daily" <* checkWordEnd
    weekly = P.string "week" <* checkWordEnd
    monthly = P.string "month" <* checkWordEnd
    -- Safe here because of `sepBy1`
    days = NESet.unsafeFromSet . Set.fromList <$> P.sepBy1 dayOfWeekP P.spaces
    noDays = P.string "no" *> P.spaces *> days

    twoOrThree = P.choice [P.string "2" $> Two, P.string "3" $> Three]
    -- TODO: Should it fail when it finds numbers?
    weeks = P.option One $ P.try (P.spaces *> twoOrThree)

type Name = Text

data Habit
  = Habit Name Frequency
  deriving (Show)

instance Eq Habit where
  Habit nameA _ == Habit nameB _ = nameA == nameB

instance ToText Habit where
  toText (Habit name freq) = name <> " {" <> toText freq <> "}"
