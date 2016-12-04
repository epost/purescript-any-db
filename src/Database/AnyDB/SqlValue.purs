module Database.AnyDB.SqlValue
  ( SqlValue()
  , class IsSqlValue
  , toSql
  ) where

import Data.Date (Date)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Node.Buffer (Buffer)
import Prelude ((<<<))

foreign import data SqlValue :: *

class IsSqlValue a where
  toSql :: a -> SqlValue

instance isSqlValueString :: IsSqlValue String where
  toSql = unsafeToSqlValue

instance isSqlValueNumber :: IsSqlValue Number where
  toSql = unsafeToSqlValue

instance isSqlValueInt :: IsSqlValue Int where
  toSql = unsafeToSqlValue <<< toNumber

instance isSqlValueBuffer :: IsSqlValue Buffer where
  toSql = unsafeToSqlValue

instance isSqlValueMaybe :: (IsSqlValue a) => IsSqlValue (Maybe a) where
  toSql Nothing = nullSqlValue
  toSql (Just x) = toSql x

instance isSqlValueDate :: IsSqlValue Date where 
  toSql = unsafeToSqlValue

foreign import unsafeToSqlValue :: forall a. a -> SqlValue

foreign import nullSqlValue :: SqlValue
