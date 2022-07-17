module Misc exposing (..)


within : number -> number -> number -> Bool
within min max val =
  if min <= val && val <= max then
    True
  else
    False

duple : a -> (a, a)
duple a = (a, a)

