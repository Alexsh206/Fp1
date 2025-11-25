{-# LANGUAGE OverloadedStrings #-}

module InitDB where

import Database.SQLite.Simple (Connection, execute_)

initDB :: Connection -> IO ()
initDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS Students (id INTEGER PRIMARY KEY AUTOINCREMENT, full_name TEXT NOT NULL, group_name TEXT NOT NULL, course INTEGER NOT NULL)"
    execute_ conn "CREATE TABLE IF NOT EXISTS Teachers (id INTEGER PRIMARY KEY AUTOINCREMENT, full_name TEXT NOT NULL, department TEXT NOT NULL)"
    execute_ conn "CREATE TABLE IF NOT EXISTS Sections (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, coach_id INTEGER, FOREIGN KEY(coach_id) REFERENCES Teachers(id))"
    execute_ conn "CREATE TABLE IF NOT EXISTS SectionMembers (id INTEGER PRIMARY KEY AUTOINCREMENT, section_id INTEGER NOT NULL, student_id INTEGER, teacher_id INTEGER, FOREIGN KEY(section_id) REFERENCES Sections(id), FOREIGN KEY(student_id) REFERENCES Students(id), FOREIGN KEY(teacher_id) REFERENCES Teachers(id))"
    execute_ conn "CREATE TABLE IF NOT EXISTS SectionSchedule (id INTEGER PRIMARY KEY AUTOINCREMENT, section_id INTEGER NOT NULL, weekday TEXT NOT NULL, start_time TEXT NOT NULL, end_time TEXT NOT NULL, FOREIGN KEY(section_id) REFERENCES Sections(id))"
    execute_ conn "CREATE TABLE IF NOT EXISTS Competitions (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, date TEXT NOT NULL, location TEXT NOT NULL)"
