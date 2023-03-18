module Utils.List exposing (..)


type alias Zipper a
  = (List a, List a)


zipperMap : (Zipper a -> a -> b) -> List a -> List b
zipperMap f list =
  let
    aux left l =
      case l of
        [] -> []
        head :: tail ->
          f (left, tail) head :: aux (left ++ [head]) tail
  in
  aux [] list


forkPrefix : List a -> List a -> (List a, List a, List a)
forkPrefix list1 list2 =
  let
    aux acc l1 l2 =
      case (l1, l2) of
        (h1 :: t1, h2 :: t2) ->
          if h1 == h2 then
            aux (h1 :: acc) t1 t2
          else
            (List.reverse acc, l1, l2)
        
        _ ->
          (List.reverse acc, l1, l2)
  in
  aux [] list1 list2


forkSuffix : List a -> List a -> (List a, List a, List a)
forkSuffix list1 list2 =
  let (l, l1, l2) = forkPrefix (List.reverse list1) (List.reverse list2) in
  (List.reverse l, List.reverse l1, List.reverse l2)

longestCommonPrefix : List a -> List a -> List a
longestCommonPrefix l1 l2 =
  let (prefix, _, _) = forkPrefix l1 l2 in
  prefix


longestCommonSuffix : List a -> List a -> List a
longestCommonSuffix l1 l2 =
  let (suffix, _, _) = forkSuffix l1 l2 in
  suffix