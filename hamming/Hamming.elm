module Hamming exposing (..)

import List
import Maybe
import String


distance : String -> String -> Maybe Int
distance str1 str2 =
    let
        ( len1, len2 ) =
            ( String.length str1, String.length str2 )
    in
        if len1 < len2 || len1 > len2 then
            Maybe.Nothing
        else
            let
                ( list1, list2 ) =
                    ( String.split "" str1, String.split "" str2 )
            in
                List.map2 (/=) list1 list2
                    |> List.foldl
                        (\x acc ->
                            if x then
                                acc + 1
                            else
                                acc
                        )
                        0
                    |> Maybe.Just
