module Triangle exposing (..)
import List exposing (all, any, member)

hasZeroes: number -> Bool
hasZeroes x =
  x <= 0

unequal: Float -> Float -> Float -> Bool
unequal num1 num2 num3 =
  let
    case1 = (num1 + num2) >= num3
    case2 = (num1 + num3) >= num2
    case3 = (num2 + num3) >= num1
  in
    if case1 && case2 && case3 then
      False
    else
      True

equilateral: Float -> Float -> Float -> Bool
equilateral num1 num2 num3 =
  if all (\x -> x == num1) [num1, num2, num3] then
    True
  else
    False

isosceles: Float -> Float -> Float -> Bool
isosceles num1 num2 num3 =
  let
    case1 = num1 == num2
    case2 = num1 == num3
    case3 = num2 == num3
  in
    if case1 || case2 || case3 then
      True
    else
      False

scalene: Float -> Float -> Float -> Bool
scalene num1 num2 num3 =
  let
    case1 = num1 /= num2
    case2 = num1 /= num3
    case3 = num2 /= num3
  in
    if case1 && case2 && case3 then
      True
    else
      False

triangleKind: Float -> Float -> Float -> Result String String
triangleKind num1 num2 num3 =
  let
    list = [num1, num2, num3]
    violates = unequal num1 num2 num3
    invalid = any hasZeroes list
    eq = equilateral num1 num2 num3
    iso = isosceles num1 num2 num3
    sca = scalene num1 num2 num3
  in
    if invalid then
      Err "Invalid lengths"
    else if violates then
      Err "Violates inequality"
    else if eq then
      Ok "equilateral"
    else if iso then
      Ok "isosceles"
    else if sca then
      Ok "scalene"
    else
      Ok "Some kind of Triangle"
