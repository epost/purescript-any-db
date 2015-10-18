"use strict";

// module Database.AnyDB
/* eslint-env node*/
/* eslint "no-underscore-dangle": 0 */

exports.connect_ = function connect_(conString) {
  return function(success, error) {
    var anyDB = require("any-db");
    anyDB.createConnection(conString, function(err, con) {
      if (err) {
        error(err);
      } else {
        success(con);
      }
    });
  };
};

exports.runQuery_ = function runQuery_(queryStr) {
  return function(con) {
    return function(success, error) {
      con.query(queryStr, function(err, result) {
        if (err) {
          error(err);
        } else {
          success(result.rows);
        }
      });
    };
  };
};

exports.runQuery = function runQuery(queryStr) {
  return function(params) {
    return function(con) {
      return function(success, error) {
        con.query(queryStr, params, function(err, result) {
          if (err) {return error(err); }
          success(result.rows);
        });
      };
    };
  };
};

exports.runQueryValue_ = function runQueryValue_(queryStr) {
  return function(con) {
    return function(success, error) {
      con.query(queryStr, function(err, result) {
        if (err) {return error(err); }
        success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
      });
    };
  };
};

exports.runQueryValue = function runQueryValue(queryStr) {
  return function(params) {
    return function(con) {
      return function(success, error) {
        con.query(queryStr, params, function(err, result) {
          if (err) {return error(err); }
          success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
        });
      };
    };
  };
};

exports.close = function close(con) {
  return function() {
    con.end();
  };
};
