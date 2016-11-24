module Test.Shared where

import Prelude
import Data.Foreign.Class (class IsForeign, readProp)

data Artist = Artist
  { name :: String
  , year :: Number
  }

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

instance artistIsForeign :: IsForeign Artist where
  read obj = do
    n <- readProp "name" obj
    y <- readProp "year" obj
    pure $ Artist { name: n, year: y }

instance artistEq :: Eq Artist where
  eq (Artist {name: n1, year: y1}) (Artist {name: n2, year: y2}) = n1 == n2 && y1 == y2
