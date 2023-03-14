module Utils.List exposing (..)


type alias Zipper a
  = (List a, List a)


zipMap : (Zipper a -> a -> b) -> List a -> List b
zipMap f list =
  let
    aux left l =
      case l of
        [] -> []
        head :: tail ->
          f (left, tail) head :: aux (left ++ [head]) tail
  in
  aux [] list