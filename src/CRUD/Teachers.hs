{-# LANGUAGE OverloadedStrings #-}

module CRUD.Teachers where

import Models
import Database.SQLite.Simple

addTeacher :: Connection -> Teacher -> IO ()
addTeacher conn (Teacher _ n d) =
  execute conn "INSERT INTO Teachers (full_name, department) VALUES (?,?)" (n, d)

getTeachers :: Connection -> IO [Teacher]
getTeachers conn =
  query_ conn "SELECT id, full_name, department FROM Teachers"

deleteTeacher :: Connection -> Int -> IO ()
deleteTeacher conn tid =
  execute conn "DELETE FROM Teachers WHERE id=?" (Only tid)
