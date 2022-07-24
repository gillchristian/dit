{-# LANGUAGE OverloadedStrings #-}

module Dit.Lib where

import Control.Monad.IO.Class (liftIO)
import Dit.Env
import Dit.Event
import Dit.Habit

run :: Maybe FilePath -> IO ()
run = runApp $ do
  createEventsTable

  let habit = Habit "Testing" Daily

  addHabit habit

  (liftIO . print) =<< getAllByHabit habit

  (liftIO . print) =<< trackHabit habit

  (liftIO . print) =<< getAllEvents
