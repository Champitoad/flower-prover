module Model.Flower exposing (..)

import Model.Formula as Formula exposing (..)

import Utils.List


type Flower
  = Formula Formula
  | Flower Garden (List Garden)

type alias Bouquet
  = List Flower

type Garden
  = Garden Bouquet


decompose : Formula -> Bouquet
decompose formula =
  case formula of
    Atom _ ->
      [Formula formula]

    Truth ->
      []
    
    Falsity ->
      [Flower (Garden []) []]
    
    And f1 f2 ->
      [Formula f1, Formula f2]
    
    Or f1 f2 ->
      [ Flower
          ( Garden [] )
          [ Garden [Formula f1]
          , Garden [Formula f2] ] ]
    
    Implies f1 f2 ->
      [ Flower
          ( Garden [Formula f1] )
          [ Garden [Formula f2] ] ]
    
    Not f1 ->
      [ Flower (Garden [Formula f1]) [] ]


-- Flower zippers


type Zip
  = Bouquet Bouquet Bouquet
  | Pistil (List Garden)
  | Petal Garden (List Garden) (List Garden)

type alias Zipper
  = List Zip

-- We encode zippers as lists, where the head is the innermost context


fillZip : Zip -> Bouquet -> Bouquet
fillZip zip bouquet =
  case zip of
    Bouquet left right ->
      left ++ bouquet ++ right

    Pistil petals ->
      [Flower (Garden bouquet) petals]

    Petal pistil leftPetals rightPetals ->
      [Flower pistil (leftPetals ++ Garden bouquet :: rightPetals)]


fillZipper : Bouquet -> Zipper -> Bouquet
fillZipper =
  List.foldl fillZip


hypsZip : Zip -> Bouquet
hypsZip zip =
  case zip of
    Bouquet left right ->
      left ++ right

    Pistil _ ->
      []
    
    Petal (Garden bouquet) _ _ ->
      bouquet


hypsZipper : Zipper -> Bouquet
hypsZipper zipper =
  List.foldl (\zip acc -> hypsZip zip ++ acc) [] zipper


isHypothesis : Flower -> Zipper -> Bool
isHypothesis flower zipper =
  List.member flower (hypsZipper zipper)


justifies : Zipper -> Zipper -> Bool
justifies source destination =
  let lca = Utils.List.longestCommonSuffix source destination in
  case source of
    -- Self pollination
    Bouquet _ _ :: (Pistil _ :: grandParent as parent) ->
      lca == grandParent ||
      lca == parent

    -- Wind pollination
    Bouquet _ _ :: parent ->
      lca == parent
    
    _ ->
      False


type Polarity
  = Pos
  | Neg


invert : Polarity -> Polarity
invert polarity =
  case polarity of
    Pos -> Neg
    Neg -> Pos


type alias Context
  = { zipper : Zipper,
      polarity : Polarity }


-- Paths are a more abstract version of zippers where we only remember the
-- position as an integer, instead of the full surrounding context

type alias Path
  = List Int


-- This is useful to maintain the identity of flowers by keeping track of their
-- position, without having to remember the content of their context, nor adding
-- a stateful identifier in the model.

-- A zipper can be reconstructed by "walking down" a path in a bouquet. The path
-- does not always denote a valid branch of the bouquet, thus this operation is
-- partial.

walk : Bouquet -> Path -> Maybe (Zipper, Bouquet)
walk bouquet path =
  let
    walkFlower acc flower path_ =
      case path_ of
        [] -> Just (List.reverse acc, [flower])
        n :: tail ->
          case flower of
            Formula _ -> Nothing
            Flower (Garden bouquet_ as pistil) petals ->
              if n == 0 then
                walkBouquet (Pistil petals :: acc) bouquet_ tail
              else
                case Utils.List.pivot (n - 1) petals of
                  (l, Garden petal :: r) ->
                    walkBouquet (Petal pistil l r :: acc) petal tail
                  _ ->
                    Nothing
    
    walkBouquet acc bouquet_ path_ =
      case path_ of
        [] -> Just (List.reverse acc, bouquet_)
        n :: tail ->
          case Utils.List.pivot (n - 1) bouquet_ of
            (l, flower :: r) ->
              walkFlower (Bouquet l r :: acc) flower tail
            _ ->
              Nothing
  in
  walkBouquet [] bouquet path


-- String representation


viewFlowerText : Flower -> String
viewFlowerText flower =
  case flower of
    Formula formula ->
      Formula.toString formula
    
    Flower pistil petals ->
      let
        pistilText =
          viewGardenText pistil

        petalsText =
          petals
          |> List.map viewGardenText
          |> String.join "; "
      in
      "(" ++ pistilText ++ " ⫐ " ++ petalsText ++ ")"


viewGardenText : Garden -> String
viewGardenText (Garden bouquet) =
  bouquet
  |> List.map viewFlowerText
  |> String.join ", "


viewZipperText : Zipper -> String
viewZipperText zipper =
  fillZipper [Formula (Atom "□")] zipper
  |> List.map (viewFlowerText)
  |> String.join ", "

logZipper : String -> Zipper -> String
logZipper msg zipper =
  zipper
  |> viewZipperText
  |> Debug.log msg

logBouquet : String -> Bouquet -> String
logBouquet msg bouquet =
  bouquet
  |> List.map viewFlowerText
  |> String.join ", "
  |> Debug.log msg


-- Examples


atom : String -> Flower
atom name =
  Formula (Atom name)


entails : Bouquet -> Bouquet -> Flower
entails phi psi =
  Flower
    ( Garden phi )
    [ Garden psi ]


yinyang : Flower
yinyang =
  entails [] []


identity : Flower
identity =
  entails [atom "a"] [atom "a"]


testFlower : Flower
testFlower =
  entails
    [entails [atom "b"] [atom "c"]]
    [atom "a", entails [atom "b"] [atom "c"]]


bigFlower : Flower
bigFlower =
  Flower
    ( Garden
        [ Formula (Atom "a")
        , Flower
            ( Garden
                [ Formula (Atom "a") ] )
            [ Garden
                [ Formula (Atom "b") ],
              Garden
                [ Flower
                    ( Garden
                        [ Formula (Atom "b") ] )
                    [ Garden
                        [ Formula (Atom "c") ] ],
                  Formula (Atom "b") ] ]
        , Flower
            ( Garden
                [ Formula (Atom "d")] )
            [ Garden
                [ Formula (Atom "e") ] ] ] )
    [ Garden
        [ Formula (Atom "b")
        , Formula (Atom "a") ]
    , Garden
      [ Formula (Atom "c") ] ]
 

modusPonensCurryfied : Flower
modusPonensCurryfied =
  entails
    [entails [atom "a"] [atom "b"]]
    [entails [atom "a"] [atom "b"]]


notFalse : Flower
notFalse =
  Flower (Garden [Flower (Garden []) []]) []


criticalPair : Flower
criticalPair =
  Flower
    ( Garden
        [ Flower
            ( Garden [] )
            [ Garden [ atom "a" ]
            , Garden [ atom "b"  ] ]
        , entails [atom "a"] [atom "c"]
        , entails [atom "b"] [atom "c"] ] )
    [ Garden [ Formula (Atom "c") ] ]


orElim : Flower
orElim =
  Formula
    ( Implies
        ( And
          ( Implies (Atom "a") (Atom "c") )
          ( Implies (Atom "b") (Atom "c") ) )
        ( Implies
          ( Or (Atom "a") (Atom "b") )
          ( Atom "c" ) ) )


orElimInvertible : Flower
orElimInvertible =
  Formula
    ( Implies
        ( Implies
          ( Or (Atom "a") (Atom "b") )
          ( Atom "c" ) )
        ( And
          ( Implies (Atom "a") (Atom "c") )
          ( Implies (Atom "b") (Atom "c") ) ) )


kreiselPutnam : Flower
kreiselPutnam =
  Formula
    ( Implies
        ( Implies
            ( Not (Atom "a") )
            ( Or (Atom "b") (Atom "c") ) )
        ( Or
            ( Implies (Not (Atom "a")) (Atom "b") )
            ( Implies (Not (Atom "a")) (Atom "c") ) ) )