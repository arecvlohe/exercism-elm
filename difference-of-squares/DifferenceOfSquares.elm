module DifferenceOfSquares exposing (..)

import List exposing (sum, repeat, map)
import Basics exposing (sqrt)

range: Int -> List Int
range num =
  if num == 0 then
    []
  else
    num :: range (num - 1)

squareOfSum: Int -> Int
squareOfSum num =
  num
  |> range
  |> sum
  |> (\x -> x^2)

sumOfSquares: Int -> Int
sumOfSquares num =
  num
  |> range
  |> map (\x -> x^2)
  |> sum

difference: Int -> Int
difference num =
  squareOfSum num - sumOfSquares num
