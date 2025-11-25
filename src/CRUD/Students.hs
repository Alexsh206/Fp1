{-# LANGUAGE OverloadedStrings #-}

module CRUD.Students where

import Models
import Database.SQLite.Simple

addStudent :: Connection -> Student -> IO ()
addStudent conn (Student _ n g c) = do
  execute conn
    "INSERT INTO Students (full_name, group_name, course) VALUES (?,?,?)"
    (n, g, c)
  putStrLn "Student added."

getStudents :: Connection -> IO [Student]
getStudents conn =
  query_ conn "SELECT id, full_name, group_name, course FROM Students"

deleteStudent :: Connection -> Int -> IO ()
deleteStudent conn sid = do
  execute conn "DELETE FROM Students WHERE id=?" (Only sid)
  putStrLn "Student deleted."
