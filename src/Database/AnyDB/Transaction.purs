module Database.AnyDB.Transaction
  ( Transaction ()
  , withTransaction
  ) where

import Control.Monad.Aff
import Control.Monad.Eff
import Database.AnyDB
import Database.AnyDB.Util (finally)

foreign import data Transaction :: *

withTransaction :: forall eff a.
     (Connection -> Aff (db :: DB | eff) a)
  -> Connection
  -> Aff (db :: DB | eff) a
withTransaction p con = do
  tx  <- beginTransaction con
  res <- p con
  commitTransaction tx
  return res

foreign import beginTransaction """
  function beginTransaction(con) {
    return function(success, error) {
      var begin = require('any-db-transaction');
      begin(con, function(err, tx) {
         if (err) {
           error(err);
         } else {
           success(tx, error);
         }
      });
    };
  }
  """ :: forall eff. Connection -> Aff (db :: DB | eff) Transaction

foreign import commitTransaction """
  function commitTransaction(tx) {
    return function(success, error) {
      tx.commit(success, error);
    };
  }
  """ :: forall eff. Transaction -> Aff (db :: DB | eff) Unit
