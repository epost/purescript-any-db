module Test.Sqlite3 where

import Database.AnyDB
import Control.Monad.Aff (Aff)
import Control.Monad.State.Trans (StateT)
import Data.Identity (Identity)
import Prelude (Unit, bind)
import Test.Shared (Artist(..))
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

connectionInfo :: ConnectionInfo
connectionInfo = Sqlite3 { filename: "test"
                         , memory: true }

main :: forall eff.
  StateT
    (Array
       (Group
          (Aff
             ( db :: DB
             | eff
             )
             Unit
          )
       )
    )
    Identity
    Unit
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
