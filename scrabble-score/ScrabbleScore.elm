module ScrabbleScore exposing (..)

import String
import List
import Regex


contains : String -> String -> Bool
contains regex str =
    Regex.contains (Regex.regex regex) str


scoreWord : String -> Int
scoreWord str =
    str
        |> String.toLower
        |> String.split ""
        |> List.foldl
            (\v acc ->
                if contains "[aeioulnrst]" v then
                    acc + 1
                else if contains "[dg]" v then
                    acc + 2
                else if contains "[bcmp]" v then
                    acc + 3
                else if contains "[fhmvwy]" v then
                    acc + 4
                else if contains "[k]" v then
                    acc + 5
                else if contains "[jx]" v then
                    acc + 8
                else if contains "[qz]" v then
                    acc + 10
                else
                    acc + 0
            )
            0
