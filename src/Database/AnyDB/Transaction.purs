module Database.AnyDB.Transaction where

import Control.Alt
import Control.Bind ((<=<))
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Trans
import Data.Array
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Data.Traversable (sequence)
import Database.AnyDB
import Database.AnyDB.Util (finally)

foreign import data Transaction :: *

withTransaction :: forall eff a.
                   ConnectionInfo
                -> (Connection -> Aff (db :: DB | eff) a)
                ->                Aff (db :: DB | eff) a
withTransaction ci p =
  withConnection ci $ \con -> do
    tx   <- _beginTransaction con
    res2 <- p con
    _commitTransaction tx
    return res2

foreign import _beginTransaction """
  function _beginTransaction(con) {
    return function(success, error) {
      var anyDB = require('any-db'),
          begin = require('any-db-transaction');

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

foreign import _commitTransaction """
  function _commitTransaction(tx) {
    return function(success, error) {
        tx.commit(success, error);
    };
  }
  """ :: forall eff. Transaction -> Aff (db :: DB | eff) Unit


