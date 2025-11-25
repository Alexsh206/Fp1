{-# LANGUAGE OverloadedStrings #-}

module Main where

import DB
import InitDB
import Models
import Database.SQLite.Simple (Connection)

-- CRUD modules
import CRUD.Students
import CRUD.Teachers
import CRUD.Sections
import CRUD.Members
import CRUD.Schedule
import CRUD.Competitions

import Data.Text (pack)
import System.IO (hSetEncoding, stdout, utf8)

-- ========== MAIN MENU ==========
main :: IO ()
main = do
    hSetEncoding stdout utf8
    conn <- connectDB
    initDB conn   -- auto create tables

    mainMenu conn


-- ========== MAIN MENU LOOP ==========
mainMenu :: Connection -> IO ()
mainMenu conn = do
    putStrLn "\n=== SPORT FACULTY INFORMATION SYSTEM ==="
    putStrLn "1. Students"
    putStrLn "2. Teachers"
    putStrLn "3. Sections"
    putStrLn "4. Section Members"
    putStrLn "5. Section Schedule"
    putStrLn "6. Competitions"
    putStrLn "0. Exit"
    putStrLn "Enter choice:"

    choice <- getLine
    case choice of
        "1" -> studentsMenu conn
        "2" -> teachersMenu conn
        "3" -> sectionsMenu conn
        "4" -> membersMenu conn
        "5" -> scheduleMenu conn
        "6" -> competitionsMenu conn
        "0" -> putStrLn "Goodbye!"
        _   -> do
            putStrLn "Invalid choice."
            mainMenu conn


--------------------------------------------------------
-- STUDENTS MENU
--------------------------------------------------------
studentsMenu :: Connection -> IO ()
studentsMenu conn = do
    putStrLn "\n--- STUDENTS MENU ---"
    putStrLn "1. Show all students"
    putStrLn "2. Add student"
    putStrLn "3. Delete student"
    putStrLn "0. Back"
    putStrLn "Enter choice:"

    c <- getLine
    case c of
        "1" -> do
            st <- getStudents conn
            print st
            studentsMenu conn
        "2" -> do
            putStrLn "Enter full name:"
            name <- getLine
            putStrLn "Enter group:"
            group <- getLine
            putStrLn "Enter course number:"
            course <- read <$> getLine

            addStudent conn (Student 0 (pack name) (pack group) course)
            putStrLn "Student added."
            studentsMenu conn
        "3" -> do
            putStrLn "Enter student ID:"
            sid <- read <$> getLine
            deleteStudent conn sid
            studentsMenu conn
        "0" -> mainMenu conn
        _   -> do
            putStrLn "Invalid choice."
            studentsMenu conn


--------------------------------------------------------
-- TEACHERS MENU
--------------------------------------------------------
teachersMenu :: Connection -> IO ()
teachersMenu conn = do
    putStrLn "\n--- TEACHERS MENU ---"
    putStrLn "1. Show all teachers"
    putStrLn "2. Add teacher"
    putStrLn "3. Delete teacher"
    putStrLn "0. Back"

    c <- getLine
    case c of
        "1" -> do
            ts <- getTeachers conn
            print ts
            teachersMenu conn
        "2" -> do
            putStrLn "Full name:"
            name <- getLine
            putStrLn "Department:"
            dept <- getLine
            addTeacher conn (Teacher 0 (pack name) (pack dept))
            teachersMenu conn
        "3" -> do
            putStrLn "Teacher ID:"
            tid <- read <$> getLine
            deleteTeacher conn tid
            teachersMenu conn
        "0" -> mainMenu conn
        _ -> teachersMenu conn


--------------------------------------------------------
-- SECTIONS MENU
--------------------------------------------------------
sectionsMenu :: Connection -> IO ()
sectionsMenu conn = do
    putStrLn "\n--- SECTIONS MENU ---"
    putStrLn "1. Show all sections"
    putStrLn "2. Add section"
    putStrLn "3. Delete section"
    putStrLn "0. Back"

    c <- getLine
    case c of
        "1" -> do
            xs <- getSections conn
            print xs
            sectionsMenu conn
        "2" -> do
            putStrLn "Section name:"
            name <- getLine
            putStrLn "Coach ID (or blank):"
            coachStr <- getLine
            let coach = if null coachStr then Nothing else Just (read coachStr)
            addSection conn (Section 0 (pack name) coach)
            sectionsMenu conn
        "3" -> do
            putStrLn "Section ID:"
            sid <- read <$> getLine
            deleteSection conn sid
            sectionsMenu conn
        "0" -> mainMenu conn
        _ -> sectionsMenu conn


--------------------------------------------------------
-- MEMBERS MENU
--------------------------------------------------------
membersMenu :: Connection -> IO ()
membersMenu conn = do
    putStrLn "\n--- MEMBERS MENU ---"
    putStrLn "1. Show all members"
    putStrLn "2. Add member"
    putStrLn "3. Delete member"
    putStrLn "0. Back"

    c <- getLine
    case c of
        "1" -> do
            xs <- getMembers conn
            print xs
            membersMenu conn
        "2" -> do
            putStrLn "Section ID:"
            sid <- read <$> getLine
            putStrLn "Student ID (or blank):"
            st <- getLine
            putStrLn "Teacher ID (or blank):"
            th <- getLine
            addMember conn (Member 0 sid (if null st then Nothing else Just(read st))
                                    (if null th then Nothing else Just(read th)))
            membersMenu conn
        "3" -> do
            putStrLn "Member ID:"
            mid <- read <$> getLine
            deleteMember conn mid
            membersMenu conn
        "0" -> mainMenu conn
        _ -> membersMenu conn


--------------------------------------------------------
-- SCHEDULE MENU
--------------------------------------------------------
scheduleMenu :: Connection -> IO ()
scheduleMenu conn = do
    putStrLn "\n--- SCHEDULE MENU ---"
    putStrLn "1. Show schedule"
    putStrLn "2. Add schedule entry"
    putStrLn "3. Delete schedule entry"
    putStrLn "0. Back"

    c <- getLine
    case c of
        "1" -> do
            xs <- getSchedule conn
            print xs
            scheduleMenu conn
        "2" -> do
            putStrLn "Section ID:"
            sid <- read <$> getLine
            putStrLn "Weekday:"
            wd <- getLine
            putStrLn "Start time (HH:MM):"
            st <- getLine
            putStrLn "End time (HH:MM):"
            en <- getLine
            addSchedule conn (Schedule 0 sid (pack wd) (pack st) (pack en))
            scheduleMenu conn
        "3" -> do
            putStrLn "Schedule ID:"
            sid <- read <$> getLine
            deleteSchedule conn sid
            scheduleMenu conn
        "0" -> mainMenu conn
        _ -> scheduleMenu conn


--------------------------------------------------------
-- COMPETITIONS MENU
--------------------------------------------------------
competitionsMenu :: Connection -> IO ()
competitionsMenu conn = do
    putStrLn "\n--- COMPETITIONS MENU ---"
    putStrLn "1. Show all competitions"
    putStrLn "2. Add competition"
    putStrLn "3. Delete competition"
    putStrLn "0. Back"

    c <- getLine
    case c of
        "1" -> do
            xs <- getCompetitions conn
            print xs
            competitionsMenu conn
        "2" -> do
            putStrLn "Title:"
            title <- getLine
            putStrLn "Date (YYYY-MM-DD):"
            date <- getLine
            putStrLn "Location:"
            loc <- getLine
            addCompetition conn (Competition 0 (pack title) (pack date) (pack loc))
            competitionsMenu conn
        "3" -> do
            putStrLn "Competition ID:"
            cid <- read <$> getLine
            deleteCompetition conn cid
            competitionsMenu conn
        "0" -> mainMenu conn
        _ -> competitionsMenu conn
