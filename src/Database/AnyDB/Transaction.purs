module Database.AnyDB.Transaction
  ( Transaction ()
  , withTransaction
  ) where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Database.AnyDB

foreign import data Transaction :: *

withTransaction :: forall eff a.
     (Connection -> Aff (db :: DB | eff) a)
  -> Connection
  -> Aff (db :: DB | eff) a
withTransaction p con = do
  tx  <- beginTransaction con
  res <- p con
  commitTransaction tx
  return res

foreign import beginTransaction :: forall eff. Connection -> Aff (db :: DB | eff) Transaction

foreign import commitTransaction :: forall eff. Transaction -> Aff (db :: DB | eff) Unit
