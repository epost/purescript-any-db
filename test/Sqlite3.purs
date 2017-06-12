module Test.Sqlite3 where

import Prelude (Unit, bind, discard)
import Test.Shared (Artist(..))
import Database.AnyDB (ConnectionInfo(..), DB, Query(..), execute_, query_, withConnection)

import Test.Spec                  (Spec, describe, it)
import Test.Spec.Assertions       (shouldEqual)

connectionInfo :: ConnectionInfo
connectionInfo = Sqlite3 { filename: "test"
                         , memory: true }

main :: forall r. Spec (db :: DB | r) Unit
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
