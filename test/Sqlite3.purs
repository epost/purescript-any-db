module Test.Sqlite3 where

import Prelude
import Test.Shared
import Test.Unit
import Control.Monad.Aff
import Database.AnyDB

connectionInfo = Sqlite3 { filename: "test"
                         , memory: true }

main = do
  test "integration test Sqlite3 + Photobooth type" do
    assertFn "should make a db, drop it, make it again, insert an Artist row, and get it back out"
      (\done -> runAff 
        (\err -> done false) 
        (\res -> done $ res == [Artist {name: "Led Zeppelin", year: 1968.0 }]) 
        (withConnection connectionInfo \conn -> do 
          execute_ (Query "CREATE TABLE artist (name CHAR(20), year INTEGER)") conn
          execute_ (Query "DROP TABLE artist") conn
          execute_ (Query "CREATE TABLE artist (name CHAR(20), year INTEGER)") conn
          execute_ (Query "INSERT INTO artist VALUES ('Led Zeppelin', '1968')") conn
          query_   (Query "SELECT * from artist":: Query Artist) conn))
