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

-- function add(a: Int, b: Int): Int { return a + b }

add :: Int -> Int -> Int
add a b = a + b

addAnyNumber :: Num a => a -> a -> a
addAnyNumber a b = a + b
