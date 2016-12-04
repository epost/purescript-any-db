module Test.Main where

import Test.Sqlite3 as SL
import Test.Postgres as PG

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  SL.main
  --PG.main
