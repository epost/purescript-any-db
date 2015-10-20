module Test.Main where

import Test.Unit
import qualified Test.Sqlite3 as SL
import qualified Test.Postgres as PG

main = runTest do
  SL.main
  --PG.main
