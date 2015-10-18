"use strict";

// module Database.AnyDB.Pool
/* eslint-env node*/
/* eslint "no-underscore-dangle": 0 */

exports.createPoolFromString = function createPoolFromString(conString) {
  return function() {
    var anyDB = require("any-db");
    return anyDB.createPool(conString);
  };
};

exports.closePool = function closePool(pool) {
  return function() {
    return pool.close();
  };
};

exports.withPool = function withPool(pool) {
  return function(connectionToDbAction) {
    return function(handleDbResult, handleDbActionError) {

      pool._pool.acquire(function(err, con) {

        function release(connection) {
          pool._reset(connection, function(err2) {
            if (err2) {return pool.destroy(connection); }
            pool._pool.release(connection);
          });
        }

        function handleDbResultThenReleaseConnection(err2, succ) {
          handleDbResult(err2, succ);
          release(con);
        }

        function handleDbActionErrorThenReleaseConnection(err2, succ) {
          handleDbActionError(err2, succ);
          release(con);
        }

        if (err) {
          console.log("withPool$prime: error acquiring connection: ", err);
          handleDbActionError(err);
        } else {
          var dbAction = connectionToDbAction(con);
          dbAction(handleDbResultThenReleaseConnection,
                   handleDbActionErrorThenReleaseConnection);
        }
      });
    };
  };
};
