{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config (Config, configCodec) where

import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Toml (TomlCodec, (.=))
import qualified Toml

data Config = Config
  { confHome :: Text,
    confData :: Text
  }
  deriving (Eq, Show)

(?=) :: TomlCodec a -> a -> TomlCodec a
aCodec ?= def =
  Toml.dimatch (Just . Just) (Maybe.fromMaybe def) $ Toml.dioptional aCodec

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.text "home" ?= "~/.config/dit" .= confHome
    <*> Toml.text "data" ?= "data.db" .= confData
