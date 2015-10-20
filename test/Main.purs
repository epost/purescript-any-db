module Test.Main where

import qualified Test.Sqlite3 as SL
import qualified Test.Postgres as PG

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  SL.main
  --PG.main
