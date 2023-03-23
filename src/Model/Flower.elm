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


yinyang : Flower
yinyang =
  Flower (Garden []) [Garden []]


identity : Flower
identity =
  Flower
    (Garden [Formula (Atom "a")])
    [Garden [Formula (Atom "a")]]


testFlower : Flower
testFlower =
  Flower
    ( Garden
        [ Flower
            (Garden [Formula (Atom "b")])
            [Garden [Formula (Atom "c")]] ] )
    [ Garden
        [ Formula (Atom "a")
        , Flower
            (Garden [Formula (Atom "b")])
            [Garden [Formula (Atom "c")]] ]]


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
 

entails : String -> String -> Flower
entails a b =
  Flower
    ( Garden
        [ Formula (Atom a) ] )
    [ Garden
        [ Formula (Atom b) ] ]


modusPonensCurryfied : Flower
modusPonensCurryfied =
  Flower
    ( Garden [ entails "a" "b" ] ) 
    [ Garden [ entails "a" "b" ] ]


notFalse : Flower
notFalse =
  Flower (Garden [Flower (Garden []) []]) []


criticalPair : Flower
criticalPair =
  Flower
    ( Garden
        [ Flower
            ( Garden [] )
            [ Garden [ Formula (Atom "a") ]
            , Garden [ Formula (Atom "b") ] ]
        , entails "a" "c"
        , entails "b" "c" ] )
    [ Garden [ Formula (Atom "c") ] ]


orElimInvertible : Flower
orElimInvertible =
  let
    formula =
      Implies
        ( Implies
          ( Or (Atom "a") (Atom "b") )
          ( Atom "c" ) )
        ( And
          ( Implies (Atom "a") (Atom "c") )
          ( Implies (Atom "b") (Atom "c") ) )
  in
  Flower (Garden []) [Garden [Formula formula]]