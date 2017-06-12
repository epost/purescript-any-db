module Test.Main where

import Prelude (Unit)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Node.Process (PROCESS)

import Database.AnyDB (DB)

import Test.Sqlite3 as SL
--import Test.Postgres as PG

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

main :: Eff (avar :: AVAR, timer :: TIMER, console :: CONSOLE, db :: DB, process :: PROCESS) Unit
main = run [consoleReporter] do
  SL.main
  --PG.main
