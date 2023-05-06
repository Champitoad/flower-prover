module Utils.List exposing (..)


toString : (a -> String) -> List a -> String
toString print l =
  "[" ++ (l |> List.map print |> String.join ", ") ++ "]"


type alias Zipper a
  = (List a, List a)


nth : Int -> List a -> Maybe a
nth n l =
  case (l, n) of
    (x :: _, 0) -> Just x
    (_ :: tail, _) -> nth (n - 1) tail
    _ -> Nothing


pivot : Int -> List a -> Zipper a
pivot n l =
  let
    aux acc n_ l_ =
      case (l_, n_) of
        (x :: t, _) -> aux (x :: acc) (n - 1) t
        (_, 0) -> (List.reverse acc, l_)
        ([], _) -> (List.reverse acc, l_)
  in
  aux [] n l


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


zipperFoldl : (Zipper a -> a -> b -> b) -> b -> List a -> b
zipperFoldl f init list =
  let
    aux left l =
      case l of
        [] -> init
        head :: tail ->
          f (left, tail) head (aux (left ++ [head]) tail)
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


slice : Int -> Int -> List a -> List a
slice start end list =
  let
    aux acc i l =
      case l of
        [] -> acc
        x :: t -> 
          if i < start then aux acc (i + 1) t
          else if i > end then acc
          else aux (x :: acc) (i + 1) t
  in
  aux [] 0 list |> List.reverse


forall : (a -> Bool) -> List a -> Bool
forall p l =
  List.foldl (\x acc -> acc && p x) True l


exists : (a -> Bool) -> List a -> Bool
exists p l =
  List.foldl (\x acc -> acc || p x) False l


hasPrefix : (List a -> Bool) -> List a -> Bool
hasPrefix p l =
  let
    (_, result) =
      List.foldl
        (\x (pre, acc) -> (pre ++ [x], acc || p pre))
        ([], False) l
  in
  result


hasSuffix : (List a -> Bool) -> List a -> Bool
hasSuffix p l =
  hasPrefix (\pre -> p (List.reverse pre)) (List.reverse l)