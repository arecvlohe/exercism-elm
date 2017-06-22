module Accumulate exposing (..)

import List exposing (head, tail)

accumulate : (a -> b) -> List a -> List b
accumulate func list =
  case list of
    [] -> []
    head :: tail -> func head :: accumulate func tail
