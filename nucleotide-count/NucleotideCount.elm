module NucleotideCount exposing (..)

import List
import String


version : Int
version =
    2


type alias Nucleotide =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> Nucleotide
nucleotideCounts str =
    str
        |> String.split ""
        |> List.foldl
            (\v acc ->
                case v of
                    "A" ->
                        { acc | a = acc.a + 1 }

                    "C" ->
                        { acc | c = acc.c + 1 }

                    "G" ->
                        { acc | g = acc.g + 1 }

                    "T" ->
                        { acc | t = acc.t + 1 }

                    _ ->
                        acc
            )
            (Nucleotide 0 0 0 0)
