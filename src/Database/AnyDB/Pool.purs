-- See https://github.com/grncdr/node-any-db-pool
module Database.AnyDB.Pool
  ( Pool()
  , withPool
  , createPool
  , createPoolFromString
  , closePool
  ) where

import Prelude
import Control.Alt
import Control.Monad.Eff
import Control.Monad.Trans
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception(Error(), error)
import Control.Monad.Error.Class (throwError)
import Data.Either
import Data.Array
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Data.Traversable (sequence)
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
