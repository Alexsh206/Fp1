{-# LANGUAGE OverloadedStrings #-}

module Models where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text (Text)

-----------------------------
-- STUDENTS
-----------------------------
data Student = Student
  { stId :: Int
  , stName :: Text
  , stGroup :: Text
  , stCourse :: Int
  } deriving (Show)

instance FromRow Student where
  fromRow = Student <$> field <*> field <*> field <*> field

instance ToRow Student where
  toRow (Student _ n g c) = toRow (n, g, c)

-----------------------------
-- TEACHERS
-----------------------------
data Teacher = Teacher
  { tId :: Int
  , tName :: Text
  , tDept :: Text
  } deriving (Show)

instance FromRow Teacher where
  fromRow = Teacher <$> field <*> field <*> field

instance ToRow Teacher where
  toRow (Teacher _ n d) = toRow (n, d)

-----------------------------
-- SECTIONS
-----------------------------
data Section = Section
  { secId :: Int
  , secName :: Text
  , secCoach :: Maybe Int
  } deriving (Show)

instance FromRow Section where
  fromRow = Section <$> field <*> field <*> field

instance ToRow Section where
  toRow (Section _ n c) = toRow (n, c)

-----------------------------
-- MEMBERS
-----------------------------
data Member = Member
  { mId :: Int
  , mSection :: Int
  , mStudent :: Maybe Int
  , mTeacher :: Maybe Int
  } deriving (Show)

instance FromRow Member where
  fromRow = Member <$> field <*> field <*> field <*> field

instance ToRow Member where
  toRow (Member _ s st t) = toRow (s, st, t)

-----------------------------
-- SCHEDULE
-----------------------------
data Schedule = Schedule
  { schId :: Int
  , schSection :: Int
  , schWeekday :: Text
  , schStart :: Text
  , schEnd :: Text
  } deriving (Show)

instance FromRow Schedule where
  fromRow = Schedule <$> field <*> field <*> field <*> field <*> field

instance ToRow Schedule where
  toRow (Schedule _ s d st en) = toRow (s, d, st, en)

-----------------------------
-- COMPETITIONS
-----------------------------
data Competition = Competition
  { cId :: Int
  , cTitle :: Text
  , cDate :: Text
  , cLoc :: Text
  } deriving (Show)

instance FromRow Competition where
  fromRow = Competition <$> field <*> field <*> field <*> field

instance ToRow Competition where
  toRow (Competition _ t d l) = toRow (t, d, l)
