{-# LANGUAGE OverloadedStrings #-}

module CRUD.Members where

import Models
import Database.SQLite.Simple

addMember :: Connection -> Member -> IO ()
addMember conn (Member _ sec st t) =
  execute conn "INSERT INTO SectionMembers (section_id, student_id, teacher_id) VALUES (?,?,?)"
    (sec, st, t)

getMembers :: Connection -> IO [Member]
getMembers conn =
  query_ conn "SELECT id, section_id, student_id, teacher_id FROM SectionMembers"

deleteMember :: Connection -> Int -> IO ()
deleteMember conn mid =
  execute conn "DELETE FROM SectionMembers WHERE id=?" (Only mid)
