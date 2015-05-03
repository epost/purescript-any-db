module Database.AnyDB.Util where

import Control.Monad.Aff
import Control.Monad.Error.Class (throwError)
import Data.Either

-- | Compute `aff1`, followed by `aff2` regardless of whether `aff1` terminated successfully.
finally :: forall e a. Aff e a -> Aff e Unit -> Aff e a
finally aff1 aff2 = do
  x <- attempt aff1
  aff2
  either throwError pure x
