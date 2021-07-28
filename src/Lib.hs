{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Config (Config (..))
import qualified Config
import Control.Exception (SomeException (SomeException))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import Data.DateTime (DateTime)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Text.ToText (ToText (toText))
import Data.Time (getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Ditfile (Frequency (Daily), Habit)
import Habit (Habit (Habit))
import System.Directory (createDirectoryIfMissing)
import System.Directory.Home (expandTilde)
import qualified System.Exit as Sys
import System.FilePath ((</>))
import qualified System.IO as Sys
import Text.RawString.QQ

data Status
  = Active
  | Paused
  | Done
  | Failed
  deriving (Eq, Show)

instance ToText Status where
  toText Active = "active"
  toText Paused = "pause"
  toText Done = "done"
  toText Failed = "failed"

instance ToField Status where
  toField = SQLText . toText

instance FromField Status where
  fromField :: FieldParser Status
  fromField fieldValue =
    case fieldData fieldValue of
      SQLText "active" -> Ok Active
      SQLText "paused" -> Ok Paused
      SQLText "done" -> Ok Done
      SQLText "failed" -> Ok Failed
      other -> Errors [SomeException $ Incompatible (show other) "Status" ""]

data EventType
  = Add
  | Pause
  | Resume
  | Finish
  | Fail
  | SetFrequency
  | Entry
  deriving (Eq, Show)

instance ToText EventType where
  toText Add = "add"
  toText Pause = "paused"
  toText Resume = "resume"
  toText Finish = "finish"
  toText Fail = "fail"
  toText SetFrequency = "set_frequency"
  toText Entry = "entry"

instance FromField EventType where
  fromField :: FieldParser EventType
  fromField fieldValue =
    case fieldData fieldValue of
      SQLText "add" -> Ok Add
      SQLText "pause" -> Ok Pause
      SQLText "resume" -> Ok Resume
      SQLText "finish" -> Ok Finish
      SQLText "fail" -> Ok Fail
      SQLText "set_frequency" -> Ok SetFrequency
      SQLText "entry" -> Ok Entry
      other -> Errors [SomeException $ Incompatible (show other) "EventType" ""]

instance ToField EventType where
  toField :: EventType -> SQLData
  toField = SQLText . toText

data Event
  = Event EventType Habit DateTime
  deriving (Eq, Show)

instance FromRow Event where
  fromRow :: RowParser Event
  fromRow = do
    Event
      <$> field
      <*> (Habit <$> field <*> field)
      <*> field

instance ToRow Event where
  toRow :: Event -> [SQLData]
  toRow (Event eventType (Habit name freq) date) =
    toRow (name, eventType, freq, date)

-- ---

defaultConfigFile :: FilePath
defaultConfigFile = "~/.config/dit.toml"

data Env = Env
  { envConf :: Config,
    envConfigFile :: FilePath,
    envConnection :: Connection
  }

type App = ReaderT Env IO

runApp :: App () -> Maybe FilePath -> IO ()
runApp app = R.runReaderT app <=< prepareAppEnv

createEventsTable :: App ()
createEventsTable = do
  conn <- R.asks envConnection
  liftIO $ execute_ conn q
  where
    q =
      [r|
          CREATE TABLE IF NOT EXISTS habit_logs (
            name TEXT NOT NULL,
            event_type TEXT NOT NULL,
            frequency TEXT NOT NULL,
            date DATETIME NOT NULL
          );
        |]

addEvent :: Event -> App ()
addEvent event = do
  conn <- R.asks envConnection
  liftIO $ execute conn q event
  where
    q = "INSERT INTO habit_logs (name, event_type, frequency, date) VALUES (?,?,?,?);"

getAllEvents :: App [Event]
getAllEvents = do
  conn <- R.asks envConnection
  liftIO $ query_ conn q
  where
    q = "SELECT event_type, name, frequency, date FROM habit_logs;"

prepareAppEnv :: Maybe FilePath -> IO Env
prepareAppEnv mbConfigPath = do
  configPath <- expandTilde $ fromMaybe defaultConfigFile mbConfigPath

  conf <-
    Config.readConfigFile configPath >>= \case
      Right conf -> do
        home <- expandTilde $ confHome conf
        pure $ conf {confHome = home}
      Left err -> do
        TIO.hPutStrLn Sys.stderr $ "Failed to parse " <> Text.pack configPath <> "\n"
        Sys.die $ Text.unpack $ Text.strip err

  createDirectoryIfMissing True $ confHome conf

  Env conf configPath <$> open (confHome conf </> confData conf)

run :: Maybe FilePath -> IO ()
run = runApp $ do
  createEventsTable

  now <- liftIO getCurrentTime

  let event = Event Add (Habit "Testing" Daily) now

  addEvent event

  (liftIO . print) =<< getAllEvents
