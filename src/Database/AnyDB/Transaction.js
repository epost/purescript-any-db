"use strict";

// module Database.AnyDB.Transaction
/* eslint-env node*/

exports.beginTransaction = function beginTransaction(con) {
  return function(success, error) {
    var begin = require("any-db-transaction");
    begin(con, function(err, tx) {
      if (err) {
        error(err);
      } else {
        success(tx, error);
      }
    });
  };
};

exports.commitTransaction = function commitTransaction(tx) {
  return function(success, error) {
    tx.commit(success, error);
  };
};
