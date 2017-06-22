module Strain exposing (..)

import List exposing (head, tail)

keep: (a -> Bool) -> List a -> List a
keep fn list =
  case list of
    [] -> []
    head :: tail ->
      if fn head then
        head :: keep fn tail
      else
        keep fn tail

discard: (a -> Bool) -> List a -> List a
discard fn list =
  case list of
    [] -> []
    head :: tail ->
      if fn head then
        discard fn tail
      else
        head :: discard fn tail
