module Anagram exposing (..)

import Regex exposing (regex, contains, caseInsensitive)
import List exposing (filter, map, sort)
import String exposing (join, length, toLower, toList)
import Char exposing (toCode)

charCodes: String -> List Int
charCodes str =
  str
    |> toList
    |> map (\x -> toCode x)
    |> sort

detect: String -> List String -> List String
detect str list =
  let
    newList =
      list
        |> filter (\x -> length x == length str && (toLower x) /= (toLower str))
    re = "([" ++ str ++ "])"
    matches = map (\x -> map .match (Regex.find Regex.All (caseInsensitive (regex re)) x )) newList
    valid =
      matches
        |> map (\x -> join "" x)
        |> filter (\x -> length x == length str)
        |> filter (\x -> (charCodes (toLower x)) == (charCodes (toLower str)))
  in
    valid
