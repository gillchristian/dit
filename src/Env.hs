{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Env
  ( App,
    Env (..),
    runApp,
  )
where

import Config
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as Sql
import qualified System.Directory as Dir
import qualified System.Directory.Home as Dir
import qualified System.Exit as Sys
import System.FilePath ((</>))
import qualified System.IO as Sys

data Env = Env
  { envConf :: Config,
    envConfigFile :: FilePath,
    envConnection :: Sql.Connection
  }

type App = ReaderT Env IO

runApp :: App () -> Maybe FilePath -> IO ()
runApp app = R.runReaderT app <=< prepareAppEnv

defaultConfigFile :: FilePath
defaultConfigFile = "~/.config/dit.toml"

prepareAppEnv :: Maybe FilePath -> IO Env
prepareAppEnv mbConfigPath = do
  configPath <- Dir.expandTilde $ fromMaybe defaultConfigFile mbConfigPath

  conf <-
    Config.readConfigFile configPath >>= \case
      Right conf -> do
        home <- Dir.expandTilde $ confHome conf
        pure $ conf {confHome = home}
      Left err -> do
        TIO.hPutStrLn Sys.stderr $ "Failed to parse " <> Text.pack configPath <> "\n"
        Sys.die $ Text.unpack $ Text.strip err

  Dir.createDirectoryIfMissing True $ confHome conf

  Env conf configPath <$> Sql.open (confHome conf </> confData conf)
