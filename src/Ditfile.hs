{-# LANGUAGE OverloadedStrings #-}

module Ditfile
  ( DayOfWeek (..),
    Ditfile,
    Frequency (..),
    Habit (..),
    addHabit,
    dropHabit,
    encode,
    habits,
    normalize,
    parse,
    sync,
  )
where

import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List (nubBy, (\\))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ToText (ToText (..))
import Habit
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

checkWordEnd :: Parser ()
checkWordEnd = P.notFollowedBy P.alphaNum

lineSpaces :: Parser ()
lineSpaces = void $ P.many $ P.oneOf " \t"

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
    -- Unsafe is actually safe here because of `sepBy1`
    days = NESet.unsafeFromSet . Set.fromList <$> P.sepBy1 dayOfWeekP P.spaces
    noDays = P.string "no" *> P.spaces *> days

    twoOrThree = P.choice [P.string "2" $> Two, P.string "3" $> Three]
    -- TODO: Should it fail when it finds numbers?
    weeks = P.option One $ P.try (P.spaces *> twoOrThree)

habitP :: Parser Habit
habitP =
  Habit
    <$> name <* lineSpaces
    <*> freq
  where
    name = Text.stripEnd . Text.pack <$> P.many (P.noneOf "{\n")
    freq =
      P.between
        (P.string "{" *> lineSpaces)
        (lineSpaces <* P.string "}")
        frequencyP

data DitNode
  = DitComment Text
  | DitWhiteSpace Text
  | DitHabit Habit
  deriving (Eq, Show)

instance ToText DitNode where
  toText (DitComment comment) = "# " <> comment
  toText (DitWhiteSpace spaces) = spaces
  toText (DitHabit habit) = toText habit

ditNodeP :: Parser DitNode
ditNodeP =
  P.choice
    [ P.try comment,
      P.try spaces,
      habit
    ]
  where
    comment = P.string "#" *> P.optional (P.char ' ') *> commentText
    spaces = DitWhiteSpace . Text.pack <$> P.many1 P.space
    habit = DitHabit <$> habitP

    commentText = DitComment . Text.stripEnd . Text.pack <$> P.many (P.noneOf "\n")

newtype Ditfile = Ditfile {getFile :: [DitNode]}
  deriving (Eq, Show)

ditfileP :: Parser Ditfile
ditfileP = Ditfile <$> P.many ditNodeP <* P.eof

-- Internal utils --------

-- TODO: lens ?
habit_ :: DitNode -> Maybe Habit
habit_ (DitHabit h) = Just h
habit_ _ = Nothing

-- TODO: remove comments in the line of a duplicated habit ?
--       fold/State with a Set of the already visited habits (which could be done by normalize)
dedupHabits :: Ditfile -> Ditfile
dedupHabits file = Ditfile $ nubBy compareHabits $ getFile file
  where
    compareHabits (DitHabit a) (DitHabit b) = a == b
    compareHabits _ _ = False

-- Work with a Ditfile ---
normalize :: Ditfile -> Ditfile
normalize (Ditfile file) = Ditfile $ go file
  where
    go :: [DitNode] -> [DitNode]
    go [] = []
    go (DitWhiteSpace a : DitWhiteSpace b : xs) =
      go $ DitWhiteSpace (a <> b) : xs
    go (DitWhiteSpace space : xs)
      | Text.any (== '\n') space =
        let trimmed = removeExtraNewLines $ Text.dropAround (/= '\n') space
         in DitWhiteSpace trimmed : go xs
      | otherwise = DitWhiteSpace space : go xs
    go (DitHabit habit : DitComment comment : xs) =
      DitHabit habit : DitWhiteSpace " " : DitComment comment : go xs
    go (x : xs) = x : go xs

    removeExtraNewLines :: Text -> Text
    removeExtraNewLines s
      | Text.length s == Text.length replaced = replaced
      | otherwise = removeExtraNewLines replaced
      where
        replaced = Text.replace "\n\n\n" "\n\n" s

parse :: FilePath -> Text -> Either Text Ditfile
parse sourcePath content =
  bimap (Text.pack . show) (normalize . dedupHabits) $
    P.parse ditfileP sourcePath content

encode :: Ditfile -> Text
encode = foldMap toText . getFile . normalize . dedupHabits

habits :: Ditfile -> [Habit]
habits = mapMaybe habit_ . getFile . dedupHabits

addHabit :: Ditfile -> Habit -> Ditfile
addHabit (Ditfile file) habit =
  normalize $ dedupHabits $ Ditfile $ file ++ [DitWhiteSpace "n", DitHabit habit]

dropHabit :: Ditfile -> Habit -> Ditfile
dropHabit (Ditfile file) habit =
  normalize $ dedupHabits $ Ditfile $ filter (/= DitHabit habit) file

sync :: Ditfile -> [Habit] -> Ditfile
sync file toSync
  | toSync == inFile = dedupHabits file
  | otherwise =
    file
      & flip (foldl addHabit) (toSync \\ inFile)
      & flip (foldl dropHabit) (inFile \\ toSync)
      & dedupHabits
  where
    inFile = habits file
