module Database.AnyDB
  ( Query(..)
  , Connection()
  , DB()
  , ConnectionInfo(..)
  , ConnectionString()
  , PgConnectionInfo()
  , Sqlite3ConnectionInfo()
  , mkConnectionString
  , connect
  , close
  , execute, execute_
  , query, query_
  , queryValue, queryValue_
  , queryOne, queryOne_
  , withConnection
  ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Data.Either (either)
import Data.Array ((!!))
import Data.Foreign (Foreign, ForeignError)
import Data.Foreign.Class (class Decode, decode)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (head)
import Control.Monad.Aff (Aff, finally)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Traversable (sequence)

import Database.AnyDB.SqlValue (SqlValue)

newtype Query a = Query String

instance eqQuery :: Eq (Query a) where
  eq (Query a) (Query b) = a == b
instance showQuery :: Show (Query a) where
  show (Query n) = n

foreign import data Connection :: Type

foreign import data DB :: Effect

type ConnectionString = String

type PgConnectionInfo =
  { host :: String
  , db :: String
  , port :: Int
  , user :: String
  , password :: String
  }

type Sqlite3ConnectionInfo =
  { filename :: String
  , memory :: Boolean}

data ConnectionInfo = Postgres PgConnectionInfo
                    | Sqlite3 Sqlite3ConnectionInfo

mkConnectionString :: ConnectionInfo -> ConnectionString
mkConnectionString (Postgres ci) = "postgres://"
                                    <> ci.user <> ":"
                                    <> ci.password <> "@"
                                    <> ci.host <> ":"
                                    <> show ci.port <> "/"
                                    <> ci.db
mkConnectionString (Sqlite3  ci) = "sqlite3://" <> if (ci.memory) then ":memory:" else ci.filename

-- | Makes a connection to the database.
connect :: forall eff. ConnectionInfo -> Aff (db :: DB | eff) Connection
connect = connect_ <<< mkConnectionString

-- | Runs a query and returns nothing.
execute :: forall eff a. Query a -> Array SqlValue -> Connection -> Aff (db :: DB | eff) Unit
execute (Query sql) params con = void $ runQuery sql params con

-- | Runs a query and returns nothing
execute_ :: forall eff a. Query a -> Connection -> Aff (db :: DB | eff) Unit
execute_ (Query sql) con = void $ runQuery_ sql con

-- | Runs a query and returns all results.
query :: forall eff a. (Decode a) =>
         Query a -> Array SqlValue -> Connection -> Aff (db :: DB | eff) (Array a)
query (Query sql) params con = do
  rows <- runQuery sql params con
  either liftError pure $ runExcept (sequence $ decode <$> rows)

-- | Just like `query` but does not make any param replacement
query_ :: forall eff a. (Decode a) => Query a -> Connection -> Aff (db :: DB | eff) (Array a)
query_ (Query sql) con = do
  rows <- runQuery_ sql con
  either liftError pure $ runExcept (sequence $ decode <$> rows)

-- | Runs a query and returns the first row, if any
queryOne :: forall eff a. (Decode a) =>
            Query a -> Array SqlValue -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryOne (Query sql) params con = do
  rows <- runQuery sql params con
  maybe (pure Nothing) ((either liftError (pure <<< Just)) <<< runExcept) $ decode <$> (rows !! 0)

-- | Just like `queryOne` but does not make any param replacement
queryOne_ :: forall eff a. (Decode a) => Query a -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryOne_ (Query sql) con = do
  rows <- runQuery_ sql con
  maybe (pure Nothing) ((either liftError (pure <<< Just)) <<< runExcept) $ decode <$> (rows !! 0)

-- | Runs a query and returns a single value, if any.
queryValue :: forall eff a. (Decode a) =>
              Query a -> Array SqlValue -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryValue (Query sql) params con = do
  val <- runQueryValue sql params con
  pure $ either (const Nothing) Just $ runExcept (decode val)

-- | Just like `queryValue` but does not make any param replacement
queryValue_ :: forall eff a. (Decode a) => Query a -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryValue_ (Query sql) con = do
  val <- runQueryValue_ sql con
  either liftError (pure <<< Just) $ runExcept $ decode val

-- | Connects to the database, calls the provided function with the connection
-- | and returns the results.
withConnection :: forall eff a.
                  ConnectionInfo
               -> (Connection -> Aff (db :: DB | eff) a)
               -> Aff (db :: DB | eff) a
withConnection info p = do
  con <- connect info
  finally (p con) $ liftEff (close con)

liftError :: forall e a. NonEmptyList ForeignError -> Aff e a
liftError = throwError <<< error <<< show <<< head <<< unwrap

foreign import connect_ :: forall eff. ConnectionString -> Aff (db :: DB | eff) Connection

foreign import runQuery_ :: forall eff. String -> Connection -> Aff (db :: DB | eff) (Array Foreign)

foreign import runQuery :: forall eff. String -> Array SqlValue -> Connection -> Aff (db :: DB | eff) (Array Foreign)

foreign import runQueryValue_ :: forall eff. String -> Connection -> Aff (db :: DB | eff) Foreign

foreign import runQueryValue :: forall eff. String -> Array SqlValue -> Connection -> Aff (db :: DB | eff) Foreign

foreign import close :: forall eff. Connection -> Eff (db :: DB | eff) Unit
