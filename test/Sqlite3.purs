module Test.Sqlite3 where

import Prelude
import Test.Shared
import Control.Monad.Aff
import Database.AnyDB

import Test.Spec                  (describe, pending, it)
import Test.Spec.Assertions       (shouldEqual)

connectionInfo = Sqlite3 { filename: "test"
                         , memory: true }

main = do
  describe "integration test Sqlite3 + Photobooth type" do
    it "should make a db, drop it, make it again, insert an Artist row, and get it back out" do
        withConnection connectionInfo \conn -> do 
          execute_ (Query "CREATE TABLE artist (name CHAR(20), year INTEGER)") conn
          execute_ (Query "DROP TABLE artist") conn
          execute_ (Query "CREATE TABLE artist (name CHAR(20), year INTEGER)") conn
          execute_ (Query "INSERT INTO artist VALUES ('Led Zeppelin', '1968')") conn
          result <- query_   (Query "SELECT * from artist":: Query Artist) conn
          result `shouldEqual` [Artist {name: "Led Zeppelin", year: 1968.0 }]
