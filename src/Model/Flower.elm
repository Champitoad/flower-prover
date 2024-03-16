module Model.Flower exposing (..)

import Model.Formula as Formula exposing (..)

import Utils.List


type alias Metadata
  = { grown : Bool
    , newAtomName : String }


type alias FormulaData
  = { metadata : Metadata
    , statement : Formula }


type alias FlowerData
  = { metadata : Metadata
    , pistil : Garden
    , petals : List Garden }


type Flower
  = Formula FormulaData
  | Flower FlowerData


type alias Bouquet
  = List Flower


type alias GardenData
  = { metadata : Metadata
    , flowers : Bouquet }


type Garden
  = Garden GardenData


isGrownFlower : Flower -> Bool
isGrownFlower flower =
  case flower of
    Formula { metadata } ->
      metadata.grown
    Flower { metadata } ->
      metadata.grown


isGrownGarden : Garden -> Bool
isGrownGarden (Garden data) =
  data.metadata.grown


naturalizeFlower : Flower -> Flower
naturalizeFlower flower =
  case flower of
    Formula ({ metadata } as data) ->
      Formula { data | metadata = { metadata | grown = False } }
    Flower ({ metadata } as data) ->
      Flower { data
             | metadata = { metadata | grown = False }
             , pistil = naturalizeGarden data.pistil
             , petals = List.map naturalizeGarden data.petals }


naturalizeGarden : Garden -> Garden
naturalizeGarden (Garden data) =
  Garden
    { data
    | metadata = { grown = False, newAtomName = "" }
    , flowers = List.map naturalizeFlower data.flowers }


harvest : Garden -> Bouquet
harvest (Garden { flowers }) =
  flowers


mkFormula : Metadata -> Formula -> Flower
mkFormula metadata statement =
  Formula (FormulaData metadata statement)

mkRealFormula : Formula -> Flower
mkRealFormula =
  mkFormula { grown = False, newAtomName = "" }


mkFakeFormula : Formula -> Flower
mkFakeFormula =
  mkFormula { grown = True, newAtomName = "" }


mkFlower : Metadata -> Garden -> List Garden -> Flower
mkFlower metadata pistil petals =
  Flower (FlowerData metadata pistil petals)


mkRealFlower : Garden -> List Garden -> Flower
mkRealFlower =
  mkFlower { grown = False, newAtomName = "" }


mkFakeFlower : Garden -> List Garden -> Flower
mkFakeFlower =
  mkFlower { grown = True, newAtomName = "" }


{-| Make a flower from a pistil and a list of petals.
    
    fl [a"a", a"b"] [[a"c"], [a"d"]]
-}
fl : Bouquet -> List Bouquet -> Flower
fl pistil petals =
  mkRealFlower (mkRealGarden pistil) (List.map mkRealGarden petals)


mkGarden : Metadata -> Bouquet -> Garden
mkGarden metadata flowers =
  Garden (GardenData metadata flowers)


mkRealGarden : Bouquet -> Garden
mkRealGarden =
  mkGarden { grown = False, newAtomName = "" }


mkFakeGarden : Bouquet -> Garden
mkFakeGarden =
  mkGarden { grown = True, newAtomName = "" }


decompose : Formula -> Bouquet
decompose formula =
  case formula of
    Atom _ ->
      [mkRealFormula formula]

    Truth ->
      []
    
    Falsity ->
      [mkRealFlower (mkRealGarden []) []]
    
    And f1 f2 ->
      [mkRealFormula f1, mkRealFormula f2]
    
    Or f1 f2 ->
      [ mkRealFlower
          ( mkRealGarden [] )
          [ mkRealGarden [mkRealFormula f1]
          , mkRealGarden [mkRealFormula f2] ] ]
    
    Implies f1 f2 ->
      [ mkRealFlower
          ( mkRealGarden [mkRealFormula f1] )
          [ mkRealGarden [mkRealFormula f2] ] ]
    
    Not f1 ->
      [ mkRealFlower (mkRealGarden [mkRealFormula f1]) [] ]


-- Flower zippers


type alias BouquetData
  = { left : Bouquet
    , right : Bouquet }


type alias PistilData
  = { metadata : Metadata
    , pistilMetadata : Metadata
    , petals : List Garden }


type alias PetalData
  = { metadata : Metadata
    , petalMetadata : Metadata
    , pistil : Garden
    , left : List Garden
    , right : List Garden }


type Zip
  = Bouquet BouquetData
  | Pistil PistilData
  | Petal PetalData


type alias Zipper
  = List Zip


-- We encode zippers as lists, where the head is the innermost context


isGrownZip : Zip -> Bool
isGrownZip zip =
  case zip of
    Pistil { metadata, pistilMetadata } ->
      metadata.grown || pistilMetadata.grown
    Petal { metadata, petalMetadata } ->
      metadata.grown || petalMetadata.grown
    _ ->
      False


isGrownZipper : Zipper -> Bool
isGrownZipper zipper =
  Utils.List.exists isGrownZip zipper


mkBouquet : Bouquet -> Bouquet -> Zip
mkBouquet left right =
  Bouquet { left = left, right = right }


mkPistil : Metadata -> Metadata -> List Garden -> Zip
mkPistil metadata pistilMetadata petals =
  Pistil { metadata = metadata
         , pistilMetadata = pistilMetadata
         , petals = petals }


mkPetal : Metadata -> Metadata -> Garden -> List Garden -> List Garden -> Zip
mkPetal metadata petalMetadata pistil left right =
  Petal { metadata = metadata
        , petalMetadata = petalMetadata
        , pistil = pistil
        , left = left
        , right = right }


fillZip : Zip -> Bouquet -> Bouquet
fillZip zip bouquet =
  case zip of
    Bouquet { left, right } ->
      left ++ bouquet ++ right

    Pistil { metadata, pistilMetadata, petals } ->
      [mkFlower metadata (mkGarden pistilMetadata bouquet) petals]

    Petal { metadata, petalMetadata, pistil, left, right } ->
      [mkFlower metadata pistil (left ++ mkGarden petalMetadata bouquet :: right)]


fillZipper : Bouquet -> Zipper -> Bouquet
fillZipper =
  List.foldl fillZip


hypsZip : Zip -> Bouquet
hypsZip zip =
  case zip of
    Bouquet { left, right } ->
      left ++ right

    Pistil _ ->
      []
    
    Petal { pistil } ->
      harvest pistil


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
    Bouquet _ :: (Pistil _ :: grandParent as parent) ->
      lca == grandParent ||
      lca == parent

    -- Wind pollination
    Bouquet _ :: parent ->
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
            Flower { metadata, pistil, petals } ->
              let (Garden pistilData) = pistil in
              if n == 0 then
                walkBouquet (mkPistil metadata pistilData.metadata petals :: acc) (harvest pistil) tail
              else
                case Utils.List.pivot (n - 1) petals of
                  (l, Garden petalData :: r) ->
                    walkBouquet (mkPetal metadata petalData.metadata pistil l r :: acc) petalData.flowers tail
                  _ ->
                    Nothing
    
    walkBouquet acc bouquet_ path_ =
      case path_ of
        [] -> Just (List.reverse acc, bouquet_)
        n :: tail ->
          case Utils.List.pivot (n - 1) bouquet_ of
            (l, flower :: r) ->
              walkFlower (mkBouquet l r :: acc) flower tail
            _ ->
              Nothing
  in
  walkBouquet [] bouquet path


-- Also there is a forgetful functor from zippers to paths


zipToInt : Zip -> Int
zipToInt zip =
  case zip of
    Bouquet { left } ->
      List.length left
    Pistil _ ->
      0
    Petal { left } ->
      1 + List.length left


zipperToPath : Zipper -> Path
zipperToPath =
  List.map zipToInt


-- String representation


viewFlowerText : Flower -> String
viewFlowerText flower =
  case flower of
    Formula { statement } ->
      Formula.toString statement
    
    Flower { pistil, petals } ->
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
viewGardenText (Garden { flowers }) =
  flowers
  |> List.map viewFlowerText
  |> String.join ", "


viewZipperText : Zipper -> String
viewZipperText zipper =
  fillZipper [mkRealFormula (Atom "□")] zipper
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
  mkRealFormula (Atom name)


{-| Make an atomic flower from a string.
    
    a "foo" == atom "foo"
-}
a : String -> Flower
a = atom


entails : Bouquet -> Bouquet -> Flower
entails phi psi =
  mkRealFlower
    ( mkRealGarden phi )
    [ mkRealGarden psi ]


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
  mkRealFlower
    ( mkRealGarden
        [ mkRealFormula (Atom "a")
        , mkRealFlower
            ( mkRealGarden
                [ mkRealFormula (Atom "a") ] )
            [ mkRealGarden
                [ mkRealFormula (Atom "b") ],
              mkRealGarden
                [ mkRealFlower
                    ( mkRealGarden
                        [ mkRealFormula (Atom "b") ] )
                    [ mkRealGarden
                        [ mkRealFormula (Atom "c") ] ],
                  mkRealFormula (Atom "b") ] ]
        , mkRealFlower
            ( mkRealGarden
                [ mkRealFormula (Atom "d")] )
            [ mkRealGarden
                [ mkRealFormula (Atom "e") ] ] ] )
    [ mkRealGarden
        [ mkRealFormula (Atom "b")
        , mkRealFormula (Atom "a") ]
    , mkRealGarden
      [ mkRealFormula (Atom "c") ] ]
 

modusPonensCurryfied : Flower
modusPonensCurryfied =
  entails
    [entails [atom "a"] [atom "b"]]
    [entails [atom "a"] [atom "b"]]


notFalse : Flower
notFalse =
  mkRealFlower (mkRealGarden [mkRealFlower (mkRealGarden []) []]) []


criticalPair : Flower
criticalPair =
  mkRealFlower
    ( mkRealGarden
        [ mkRealFlower
            ( mkRealGarden [] )
            [ mkRealGarden [ atom "a" ]
            , mkRealGarden [ atom "b"  ] ]
        , entails [atom "a"] [atom "c"]
        , entails [atom "b"] [atom "c"] ] )
    [ mkRealGarden [ mkRealFormula (Atom "c") ] ]


orElim : Flower
orElim =
  mkRealFormula
    ( Implies
        ( And
          ( Implies (Atom "a") (Atom "c") )
          ( Implies (Atom "b") (Atom "c") ) )
        ( Implies
          ( Or (Atom "a") (Atom "b") )
          ( Atom "c" ) ) )


orElimInvertible : Flower
orElimInvertible =
  mkRealFormula
    ( Implies
        ( Implies
          ( Or (Atom "a") (Atom "b") )
          ( Atom "c" ) )
        ( And
          ( Implies (Atom "a") (Atom "c") )
          ( Implies (Atom "b") (Atom "c") ) ) )


kreiselPutnam : Flower
kreiselPutnam =
  mkRealFormula
    ( Implies
        ( Implies
            ( Not (Atom "a") )
            ( Or (Atom "b") (Atom "c") ) )
        ( Or
            ( Implies (Not (Atom "a")) (Atom "b") )
            ( Implies (Not (Atom "a")) (Atom "c") ) ) )