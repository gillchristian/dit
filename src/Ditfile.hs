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

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.List (nubBy, (\\))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ToText (ToText (..))
import Habit
import qualified Text.Parsec as P
import Text.Parsec.Extra (lineSpaces)
import Text.Parsec.Text (Parser)

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
