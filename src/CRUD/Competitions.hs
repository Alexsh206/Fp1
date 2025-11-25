{-# LANGUAGE OverloadedStrings #-}

module CRUD.Competitions where

import Models
import Database.SQLite.Simple

addCompetition :: Connection -> Competition -> IO ()
addCompetition conn (Competition _ t d l) =
  execute conn "INSERT INTO Competitions (title, date, location) VALUES (?,?,?)"
    (t, d, l)

getCompetitions :: Connection -> IO [Competition]
getCompetitions conn =
  query_ conn "SELECT id, title, date, location FROM Competitions"

deleteCompetition :: Connection -> Int -> IO ()
deleteCompetition conn cid =
  execute conn "DELETE FROM Competitions WHERE id=?" (Only cid)
