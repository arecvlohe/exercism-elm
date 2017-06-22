module Bob exposing (..)

import String exposing (isEmpty, toList, endsWith)
import Char exposing (isUpper, isDigit)
import Regex exposing (replace, regex, contains)
import List exposing (all, isEmpty)

listOfChars : String -> List Char
listOfChars string =
  string
    |> replace Regex.All (regex "[^A-Za-z]") (\_ -> "")
    |> toList

allUppercase : List Char -> Bool
allUppercase charList =
  charList
    |> all isUpper

stringOfNothing : String -> Bool
stringOfNothing string =
  string
   |> contains (regex "^[\\s\\r\\t\\n]+$")

hey : String -> String
hey string =
  if String.isEmpty string || stringOfNothing string then
    "Fine. Be that way!"
  else if endsWith "?" string && not (allUppercase (listOfChars string)) || contains (regex "\\d\\?") string then
    "Sure."
  else if allUppercase (listOfChars string) && not (List.isEmpty (listOfChars string)) then
    "Whoa, chill out!"
  else
    "Whatever."
