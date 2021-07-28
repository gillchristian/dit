{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config
  ( Config (..),
    encode,
    readConfigFile,
  )
where

import qualified Data.Bifunctor as BF
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Habit
import Toml (TomlCodec, (.=))
import qualified Toml

data Config = Config
  { confHome :: String,
    confData :: String,
    confWeekStartsOn :: Maybe DayOfWeek
  }
  deriving (Eq, Show)

(?=) :: TomlCodec a -> a -> TomlCodec a
(?=) aCodec def =
  Toml.dimatch (Just . Just) (Maybe.fromMaybe def) $ Toml.dioptional aCodec

dayOfWeek :: Toml.Key -> TomlCodec (Maybe DayOfWeek)
dayOfWeek = Toml.dimatch consumer producer . Toml.text
  where
    consumer :: Maybe DayOfWeek -> Maybe Text
    consumer (Just Sunday) = Just "sun"
    consumer (Just Monday) = Just "mon"
    consumer (Just Tuesday) = Just "tue"
    consumer (Just Wednesday) = Just "wed"
    consumer (Just Thursday) = Just "thu"
    consumer (Just Friday) = Just "fri"
    consumer (Just Saturday) = Just "sat"
    consumer Nothing = Nothing

    producer :: Text -> Maybe DayOfWeek
    producer "sun" = Just Sunday
    producer "mon" = Just Monday
    producer "tue" = Just Tuesday
    producer "wed" = Just Wednesday
    producer "thu" = Just Thursday
    producer "fri" = Just Friday
    producer "sat" = Just Saturday
    producer _ = Nothing

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.string "home" ?= "~/.config/dit" .= confHome
    <*> Toml.string "data" ?= "data.db" .= confData
    <*> dayOfWeek "week_starts_on" ?= Just Sunday .= confWeekStartsOn

-- TODO: catch exceptions (file doesn't exist)
readConfigFile :: FilePath -> IO (Either Text Config)
readConfigFile path =
  BF.first Toml.prettyTomlDecodeErrors
    <$> Toml.decodeFileEither configCodec path

encode :: Config -> Text
encode = Toml.encode configCodec
