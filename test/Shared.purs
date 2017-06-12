module Test.Shared where

import Prelude
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)

data Artist = Artist
  { name :: String
  , year :: Number
  }

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

derive instance artistGeneric :: Generic Artist _

instance artistDecode :: Decode Artist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance artistEq :: Eq Artist where
  eq (Artist {name: n1, year: y1}) (Artist {name: n2, year: y2}) = n1 == n2 && y1 == y2
