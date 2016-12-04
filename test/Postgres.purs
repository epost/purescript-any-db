module Test.Postgres where

import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Trans (StateT)
import Data.Foldable (foldMap)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Database.AnyDB (ConnectionInfo(Postgres), DB, Query(Query), close, connect, execute_, query, queryOne_, queryValue_, query_, withConnection)
import Database.AnyDB.Pool (closePool, createPool, withPool)
import Database.AnyDB.SqlValue (toSql)
import Database.AnyDB.Transaction (withTransaction)
import Prelude (class Show, Unit, bind, flip, show, ($), (<>), (>>>))
import Test.Shared (Artist(..))
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
  describe "integration test PostgreSQL + Photobooth type" do
    it "should make a db, drop it, make it again, insert an Artist row, and get it back out" do
        withConnection connectionInfo \conn -> do 
          execute_ (Query "CREATE TABLE artist (name CHAR(20), year INTEGER)") conn
          execute_ (Query "DROP TABLE artist") conn
          execute_ (Query "CREATE TABLE artist (name CHAR(20), year INTEGER)") conn
          execute_ (Query "INSERT INTO artist VALUES ('Led Zeppelin', '1968')") conn
          result <- query_   (Query "SELECT * from artist":: Query Artist) conn
          result `shouldEqual` [Artist {name: "Led Zeppelin", year: 1968.0 }]


-- !!! Move to spec format.
--main :: forall eff.
--  Eff
--    ( console :: CONSOLE
--    , db :: DB
--    | eff
--    )
--    Unit
--main = runAff logShow (const $ log "All ok") $ do
--  liftEff <<< log $ "connecting to " <> mkConnectionString connectionInfo <> "..."
--  exampleUsingWithConnection
--  exampleLowLevel
--
--  res <- attempt exampleError
--  liftEff $ either (const $ log "got an error, like we should") (const $ log "FAIL") res
--
--  exampleQueries
--  exampleUsingWithPool
--  exampleUsingWithTransaction
--  pure unit
--  --result `shouldEqual` [Artist {name: "Led Zeppelin", year: 1968.0 }]

connectionInfo :: ConnectionInfo
connectionInfo = Postgres { host: "localhost"
                          , db: "test"
                          , port: 5432
                          , user: "testuser"
                          , password: "test" }

exampleUsingWithConnection :: forall eff. Aff (console :: CONSOLE, db :: DB | eff) Unit
exampleUsingWithConnection = withConnection connectionInfo $ \c -> do
  execute_ (Query "delete from artist") c
  execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
  execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
  year <- queryValue_ (Query "insert into artist values ('Fairport Convention', 1967) returning year" :: Query Number) c
  liftEff $ log (show year)
  artists <- query_ (Query "select * from artist" :: Query Artist) c
  liftEff $ printRows artists

exampleLowLevel :: forall eff. Aff (console :: CONSOLE, db :: DB | eff) Unit
exampleLowLevel = do
  con <- connect connectionInfo
  artists <- query_ (Query "select * from artist order by name desc" :: Query Artist) con
  liftEff $ printRows artists
  liftEff $ close con

exampleError :: forall eff. Aff (db :: DB | eff) (Maybe Artist)
exampleError = withConnection connectionInfo $ \c -> do
  execute_ (Query "delete from artist") c
  execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
  queryOne_ (Query "select year from artist") c

exampleQueries :: forall eff. Aff (console :: CONSOLE, db :: DB | eff) Unit
exampleQueries = withConnection connectionInfo $ \c -> do
  liftEff $ log "Example queries with params:"
  execute_ (Query "delete from artist") c
  execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
  execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
  execute_ (Query "insert into artist values ('Toto', 1977)") c
  artists <- query (Query "select * from artist where name = $1" :: Query Artist) [toSql "Toto"] c
  liftEff $ printRows artists

exampleUsingWithPool :: forall eff. Aff (console :: CONSOLE, db :: DB | eff) Unit
exampleUsingWithPool = do
  pool <- liftEff $ createPool connectionInfo
  withPool pool $ \c -> do
    artists <- query_ (Query "select * from artist" :: Query Artist) c
    liftEff $ printRows artists
  liftEff $ closePool pool

exampleUsingWithTransaction :: forall eff. Aff (console :: CONSOLE, db :: DB | eff) Unit
exampleUsingWithTransaction =
  withConnection connectionInfo $ withTransaction \con -> do
    artists <- query_ (Query "select * from artist" :: Query Artist) con
    liftEff $ printRows artists

printRows :: forall a eff. (Show a) => Array a -> Eff (console :: CONSOLE | eff) Unit
printRows rows = log $ "result:\n" <> foldMap stringify rows
  where stringify = show >>> flip (<>) "\n"
