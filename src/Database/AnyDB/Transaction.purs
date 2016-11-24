module Database.AnyDB.Transaction
  ( Transaction ()
  , withTransaction
  ) where

import Prelude (Unit, bind, pure)
import Control.Monad.Aff (Aff)
import Database.AnyDB (Connection, DB)

foreign import data Transaction :: *

withTransaction :: forall eff a.
     (Connection -> Aff (db :: DB | eff) a)
  -> Connection
  -> Aff (db :: DB | eff) a
withTransaction p con = do
  tx  <- beginTransaction con
  res <- p con
  commitTransaction tx
  pure res

foreign import beginTransaction :: forall eff. Connection -> Aff (db :: DB | eff) Transaction

foreign import commitTransaction :: forall eff. Transaction -> Aff (db :: DB | eff) Unit
