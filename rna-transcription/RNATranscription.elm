module RNATranscription exposing (..)

import List
import Result
import String


toRNA : String -> Result Char String
toRNA str =
    str
        |> String.toList
        |> List.map
            (\v ->
                case v of
                    'A' ->
                        (Ok "U")

                    'C' ->
                        (Ok "G")

                    'G' ->
                        (Ok "C")

                    'T' ->
                        (Ok "A")

                    _ ->
                        (Err v)
            )
        |> List.foldl
            (\v acc ->
                Result.map2 (++) acc v
            )
            (Ok "")
