module Raindrops exposing (..)

import String exposing (isEmpty, concat)
import Basics exposing (toString)

isDivisibleBy3 : Int -> String
isDivisibleBy3 num =
  if num % 3 == 0 then
    "Pling"
  else
    ""

isDivisibleBy5 : Int -> String
isDivisibleBy5 num  =
  if num % 5 == 0 then
    "Plang"
  else
    ""

isDivisibleBy7 : Int -> String
isDivisibleBy7 num =
  if num % 7 == 0 then
    "Plong"
  else
    ""

raindrops : Int -> String
raindrops num =
  let
    strNum = toString num
    three  = isDivisibleBy3 num
    five   = isDivisibleBy5 num
    seven  = isDivisibleBy7 num
    str    = concat [three, five, seven]
  in
    if isEmpty str then
      strNum
    else
      str
