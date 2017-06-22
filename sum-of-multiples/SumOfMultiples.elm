module SumOfMultiples exposing (..)

import List
import Set


findMultiples : Int -> Int -> Int -> List Int
findMultiples step iterations end =
    let
        value =
            step * iterations
    in
        if value >= end then
            []
        else
            value :: findMultiples step (iterations + 1) end


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples list num =
    list
        |> List.concatMap (\v -> findMultiples v 1 num)
        |> Set.fromList
        |> Set.foldl (+) 0
