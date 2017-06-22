module Leap exposing (..)

isLeapYear : Int -> Bool
isLeapYear year =
  if (year % 4 == 0 && year % 100 /= 0) || year % 400 == 0 then
    True
  else
    False
