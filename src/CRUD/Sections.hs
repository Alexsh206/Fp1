{-# LANGUAGE OverloadedStrings #-}

module CRUD.Sections where

import Models
import Database.SQLite.Simple

addSection :: Connection -> Section -> IO ()
addSection conn (Section _ n c) =
  execute conn "INSERT INTO Sections (name, coach_id) VALUES (?,?)" (n, c)

getSections :: Connection -> IO [Section]
getSections conn =
  query_ conn "SELECT id, name, coach_id FROM Sections"

deleteSection :: Connection -> Int -> IO ()
deleteSection conn sid =
  execute conn "DELETE FROM Sections WHERE id=?" (Only sid)
