-- See https://github.com/grncdr/node-any-db-pool
module Database.AnyDB.Pool
  ( Pool()
  , withPool
  , createPool
  , createPoolFromString
  , closePool
  ) where

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
foreign import createPoolFromString """
  function createPoolFromString(conString) {
    return function() {
      var anyDB = require('any-db');
      return anyDB.createPool(conString);
    }
  }
""" :: forall eff. ConnectionString -> Eff (db :: DB | eff) Pool

-- | Close the connection pool.
foreign import closePool """
  function closePool(pool) {
    return function() {
      return pool.close();
    }
  }
""" :: forall eff. Pool -> Eff (db :: DB | eff) Unit

-- | Run a database action with a connection from the specified `Pool`.
foreign import withPool """
function withPool(pool) {
  return function(connectionToDbAction) {
    return function(handleDbResult, handleDbActionError) {

      pool._pool.acquire(function(err, con) {

        function release(connection) {
          pool._reset(connection, function(err) {
            if (err) return pool.destroy(connection);
            pool._pool.release(connection);
          });
        }

        function handleDbResultThenReleaseConnection(err, succ) {
            handleDbResult(err, succ);
            release(con);
        }

        function handleDbActionErrorThenReleaseConnection(err, succ) {
            handleDbActionError(err, succ);
            release(con);
        }

        if (err) {
          console.log('withPool$prime: error acquiring connection: ', err);
          handleDbActionError(err);
        } else {
          var dbAction = connectionToDbAction(con);
          dbAction(handleDbResultThenReleaseConnection,
                   handleDbActionErrorThenReleaseConnection);
        }
      });
    };
  };
}"""
 :: forall eff a. Pool
               -> (Connection -> Aff (db :: DB | eff) a)
               -> Aff (db :: DB | eff) a
