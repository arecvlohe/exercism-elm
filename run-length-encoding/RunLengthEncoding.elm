module RunLengthEncoding exposing (..)

import Array
import List
import Maybe
import Regex
import Result
import String
import Tuple


version : Int
version =
    2



-- This code doesn't need comments, it speaks for itself :)


encode : String -> String
encode str =
    if Regex.contains (Regex.regex " ") str then
        str
    else
        str
            |> makeIndexMap
            |> setAccValue
            |> mapEncoded
            |> makeIntoString



-- BEGIN: ENCODE HELPERS


makeIndexMap : String -> Array.Array ( Int, String )
makeIndexMap =
    String.split "" >> Array.fromList >> Array.indexedMap (,)


getPrevValue : Array.Array ( Int, String ) -> ( Int, String )
getPrevValue acc =
    acc
        |> Array.get (Array.length acc - 1)
        |> Maybe.withDefault ( 0, "" )


setPrevValue : Array.Array ( Int, String ) -> ( Int, String ) -> String -> Array.Array ( Int, String )
setPrevValue acc prev curr =
    if (Tuple.second prev) == curr then
        Array.set (Array.length acc - 1) (Tuple.mapFirst ((+) 1) prev) acc
    else
        Array.push ( 1, curr ) acc


setAccValue : Array.Array ( Int, String ) -> Array.Array ( Int, String )
setAccValue array =
    Array.foldl
        (\( i, str ) acc ->
            if i == 0 then
                Array.push ( 1, str ) acc
            else
                setPrevValue acc (getPrevValue acc) str
        )
        Array.empty
        array


mapEncoded : Array.Array ( Int, String ) -> Array.Array String
mapEncoded array =
    Array.map
        (\( i, v ) ->
            if i == 1 then
                v
            else
                toString i ++ v
        )
        array


makeIntoString : Array.Array String -> String
makeIntoString =
    Array.toList >> String.join ""



-- END: ENCODE HELPERS


decode : String -> String
decode str =
    if Regex.contains (Regex.regex " ") str then
        str
    else
        str
            |> parseEncoded
            |> intoString



-- BEGIN: DECODE HELPERS


parseEncoded : String -> List String
parseEncoded =
    List.map .match << Regex.find Regex.All (Regex.regex "(\\d+)?(\\w|\\u23F0|\\u26BD|\\u2B50)")


parseNum : String -> Int
parseNum =
    Result.withDefault 1 << String.toInt << String.slice 0 -1


intoString : List String -> String
intoString list =
    List.foldl
        (\v acc -> acc ++ String.repeat (parseNum v) (String.right 1 v))
        ""
        list
