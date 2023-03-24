module Utils.Maybe exposing (..)


isNothing : Maybe a -> Bool
isNothing x =
  case x of
    Nothing -> True
    _ -> False


isSomething : Maybe a -> Bool
isSomething =
  not << isNothing