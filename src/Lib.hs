{-# LANGUAGE OverloadedStrings #-}

module Lib (run) where

import Config (Config (..))
import qualified Config
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory.Home (expandTilde)
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified Toml

defaultConfigFile :: FilePath
defaultConfigFile = "~/.config/dit.toml"

data Env = Env
  { envConf :: Config,
    envConfigFile :: FilePath
  }
  deriving (Eq, Show)

type App = ReaderT Env IO

run :: Maybe FilePath -> IO ()
run mbConfigPath = do
  configPath <- expandTilde $ fromMaybe defaultConfigFile mbConfigPath
  eConfig <- Config.readConfigFile configPath

  conf <- case eConfig of
    Right conf -> pure conf
    Left err -> do
      TIO.hPutStrLn Sys.stderr $ "Failed to parse " <> Text.pack configPath <> "\n"
      -- TODO: pretty error formatting (insead of library errors)
      Sys.die $ Text.unpack $ Text.strip err

  TIO.putStrLn $ Config.encode conf
