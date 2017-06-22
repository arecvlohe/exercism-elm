module Pangram exposing (..)

import String exposing (toLower, toList)
import Regex exposing (..)
import Set exposing (fromList, size)

isPangram : String -> Bool
isPangram string =
  string
    |> toLower
    |> replace All (regex "[^a-z]") (\_ -> "")
    |> toList
    |> fromList
    |> size
    |> (==) 26
