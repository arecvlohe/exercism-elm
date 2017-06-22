module SpaceAge exposing (..)

type Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

toDays: Float -> Float
toDays time =
  time / 60 / 60 / 24

ageOn: Planet -> Float -> Float
ageOn planet time =
  case planet of
    Mercury -> toDays time / (0.2408467 * 365.25)
    Venus -> toDays time / (0.61519726 * 365.25)
    Earth -> toDays time / 365.25
    Mars -> toDays time / (1.8808158 * 365.25)
    Jupiter -> toDays time / (11.862615 * 365.25)
    Saturn -> toDays time / (29.447498 * 365.25)
    Uranus -> toDays time / (84.016846 * 365.25)
    Neptune -> toDays time / (164.79132 * 365.25)
