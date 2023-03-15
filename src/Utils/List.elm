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


longestCommonPrefix : List a -> List a -> List a
longestCommonPrefix l1 l2 =
  case (l1, l2) of
    (h1 :: t1, h2 :: t2) ->
      if h1 == h2 then
        h1 :: longestCommonPrefix t1 t2
      else
        []
    
    _ ->
      []


longestCommonSuffix : List a -> List a -> List a
longestCommonSuffix l1 l2 =
  longestCommonPrefix (List.reverse l1) (List.reverse l2)