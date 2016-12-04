module Test.Main where

import Test.Postgres as PG
import Test.Sqlite3 as SL
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Database.AnyDB (DB)
import Node.Process (PROCESS)
import Prelude (Unit)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: forall t4.
  Eff
    ( process :: PROCESS
    , console :: CONSOLE
    , db :: DB
    | t4
    ) Unit
main = run [consoleReporter] do
  SL.main
  --PG.main
