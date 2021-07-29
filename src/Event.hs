{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Event where

import Control.Exception (SomeException (SomeException))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Reader as R
import Data.DateTime (DateTime)
import Data.Text.ToText
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (SQLData (SQLText))
import qualified Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField (FieldParser, FromField (fromField))
import qualified Database.SQLite.Simple.FromField as Sql
import Database.SQLite.Simple.FromRow (FromRow (fromRow))
import Database.SQLite.Simple.Internal (RowParser)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import Ditfile
import Env
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
    case Sql.fieldData fieldValue of
      SQLText "active" -> Ok Active
      SQLText "paused" -> Ok Paused
      SQLText "done" -> Ok Done
      SQLText "failed" -> Ok Failed
      other -> Errors [SomeException $ Sql.Incompatible (show other) "Status" ""]

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
    case Sql.fieldData fieldValue of
      SQLText "add" -> Ok Add
      SQLText "pause" -> Ok Pause
      SQLText "resume" -> Ok Resume
      SQLText "finish" -> Ok Finish
      SQLText "fail" -> Ok Fail
      SQLText "set_frequency" -> Ok SetFrequency
      SQLText "entry" -> Ok Entry
      other -> Errors [SomeException $ Sql.Incompatible (show other) "EventType" ""]

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
      <$> Sql.field
      <*> (Habit <$> Sql.field <*> Sql.field)
      <*> Sql.field

instance ToRow Event where
  toRow :: Event -> [SQLData]
  toRow (Event eventType (Habit name freq) date) =
    toRow (name, eventType, freq, date)

-- ---

createEventsTable :: App ()
createEventsTable = do
  conn <- R.asks envConnection
  liftIO $ Sql.execute_ conn q
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

insertEventQ :: Sql.Query
insertEventQ = "INSERT INTO habit_logs (name, event_type, frequency, date) VALUES (?,?,?,?);"

allEventsQ :: Sql.Query
allEventsQ = "SELECT event_type, name, frequency, date FROM habit_logs;"

byNameQ :: Sql.Query
byNameQ =
  [r|
    SELECT event_type, name, frequency, date
    FROM habit_logs
    WHERE name = ?
    ORDER BY date DESC;
  |]

habitEntriesFromTodayQ :: Sql.Query
habitEntriesFromTodayQ =
  [r|
    SELECT event_type, name, frequency, date
    FROM habit_logs
    WHERE name = ?
    AND event_type = ?
    AND date(date) = date('now');
  |]

byNameAndEventQ :: Sql.Query
byNameAndEventQ =
  [r|
    SELECT event_type, name, frequency, date
    FROM habit_logs
    WHERE name = ?
    AND event_type = ?;
  |]

addHabit :: Habit -> App ()
addHabit habit@(Habit name _) = do
  conn <- R.asks envConnection
  now <- liftIO getCurrentTime
  es :: [Event] <- liftIO $ Sql.query conn byNameAndEventQ (name, Add)
  when (null es) $ liftIO $ Sql.execute conn insertEventQ $ Event Add habit now

trackHabit :: Habit -> App ()
trackHabit habit@(Habit name _) = do
  conn <- R.asks envConnection
  now <- liftIO getCurrentTime
  es :: [Event] <- liftIO $ Sql.query conn habitEntriesFromTodayQ (name, Entry)
  when (null es) $ liftIO $ Sql.execute conn insertEventQ $ Event Entry habit now

getAllEvents :: App [Event]
getAllEvents = do
  conn <- R.asks envConnection
  liftIO $ Sql.query_ conn q
  where
    q = "SELECT event_type, name, frequency, date FROM habit_logs;"

getAllByHabit :: Habit -> App [Event]
getAllByHabit (Habit name _) = do
  conn <- R.asks envConnection
  liftIO $ Sql.query conn byNameQ $ Sql.Only name
