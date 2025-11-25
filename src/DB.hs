{-# LANGUAGE OverloadedStrings #-}

module DB where

import Database.SQLite.Simple

connectDB :: IO Connection
connectDB = open "sport.db"