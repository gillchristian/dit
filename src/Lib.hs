{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.IO.Class (liftIO)
import Env
import Event
import Habit

run :: Maybe FilePath -> IO ()
run = runApp $ do
  createEventsTable

  let habit = Habit "Testing" Daily

  addHabit habit

  (liftIO . print) =<< getAllByHabit habit

  (liftIO . print) =<< trackHabit habit

  (liftIO . print) =<< getAllEvents
