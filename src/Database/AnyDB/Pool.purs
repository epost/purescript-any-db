-- See https://github.com/grncdr/node-any-db-pool
module Database.AnyDB.Pool
  ( Pool()
  , withPool
  , createPool
  , createPoolFromString
  , closePool
  ) where

import Prelude (Unit, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Database.AnyDB

foreign import data Pool :: *

-- | Create a connection pool.
createPool :: forall eff. ConnectionInfo -> Eff (db :: DB | eff) Pool
createPool ci = createPoolFromString $ mkConnectionString ci

-- | Create a connection pool. Remember to call `closePool`.
foreign import createPoolFromString :: forall eff. ConnectionString -> Eff (db :: DB | eff) Pool

-- | Close the connection pool.
foreign import closePool :: forall eff. Pool -> Eff (db :: DB | eff) Unit

-- | Run a database action with a connection from the specified `Pool`.
foreign import withPool :: forall eff a. Pool
               -> (Connection -> Aff (db :: DB | eff) a)
               -> Aff (db :: DB | eff) a
