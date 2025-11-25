{-# LANGUAGE OverloadedStrings #-}

module CRUD.Schedule where

import Models
import Database.SQLite.Simple

addSchedule :: Connection -> Schedule -> IO ()
addSchedule conn (Schedule _ s d st en) =
  execute conn "INSERT INTO SectionSchedule (section_id, weekday, start_time, end_time) VALUES (?,?,?,?)"
    (s, d, st, en)

getSchedule :: Connection -> IO [Schedule]
getSchedule conn =
  query_ conn "SELECT id, section_id, weekday, start_time, end_time FROM SectionSchedule"

deleteSchedule :: Connection -> Int -> IO ()
deleteSchedule conn sid =
  execute conn "DELETE FROM SectionSchedule WHERE id=?" (Only sid)
