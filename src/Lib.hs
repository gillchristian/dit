{-# LANGUAGE OverloadedStrings #-}

module Lib (run) where

import Config (Config, configCodec)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Text.IO as TIO
import qualified Toml

defaultConfigFile :: FilePath
defaultConfigFile = "~/.config/dit.toml"

data Env = Env
  { envConf :: Config,
    envConfigFile :: FilePath
  }
  deriving (Eq, Show)

type App = ReaderT Env IO

run :: IO ()
run = do
  tomlRes <- Toml.decodeFileEither configCodec "config.toml"

  case tomlRes of
    Left errs -> TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
    Right settings -> TIO.putStrLn $ Toml.encode configCodec settings
