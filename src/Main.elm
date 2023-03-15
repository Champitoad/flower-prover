module Main exposing (..)

import Utils.List
import Utils.Events exposing (..)

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events


-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = \model -> layout [] (view model) }


-- MODEL


type Flower
  = Atom String
  | Flower Garden (List Garden)

type alias Bouquet
  = List Flower

type Garden
  = Garden Bouquet


type FlowerZip
  = Hole
  | Pistil (List Garden)
  | Petal Garden (List Garden) (List Garden)

type alias Zip
  = (Bouquet, FlowerZip, Bouquet)

type alias Zipper
  = List Zip

-- We encode zippers as lists, where the head is the innermost context


fillZip : Zip -> Bouquet -> Bouquet
fillZip (left, zip, right) bouquet =
  let
    filled =
      case zip of
        Hole ->
          bouquet

        Pistil petals ->
          [Flower (Garden bouquet) petals]

        Petal pistil leftPetals rightPetals ->
          [Flower pistil (leftPetals ++ Garden bouquet :: rightPetals)]
  in
  left ++ filled ++ right


fillZipper : Bouquet -> Zipper -> Bouquet
fillZipper =
  List.foldl fillZip


hypsZip : Zip -> Bouquet
hypsZip (left, zip, right) =
  let
    hyps =
      case zip of
        Hole ->
          []

        Pistil _ ->
          []
        
        Petal (Garden bouquet) _ _ ->
          bouquet
  in
  left ++ right ++ hyps


hypsZipper : Zipper -> Bouquet
hypsZipper zipper =
  List.foldl (\zip acc -> hypsZip zip ++ acc) [] zipper


isHypothesis : Flower -> Zipper -> Bool
isHypothesis flower zipper =
  List.member flower (hypsZipper zipper)


leastCommonAncestor : Zipper -> Zipper -> Zipper
leastCommonAncestor z1 z2 =
  Utils.List.longestCommonSuffix z1 z2


type Polarity
  = Pos
  | Neg


negate : Polarity -> Polarity
negate polarity =
  case polarity of
    Pos -> Neg
    Neg -> Pos


type alias Context
  = { zipper : Zipper,
      polarity : Polarity }


type alias Selection
  = List Zipper
 

type ProofInteraction
  = Justifying
  | Importing Flower -- source statement
  | Fencing Selection


type UIMode
  = ProofMode ProofInteraction
  | EditMode


type alias Model
  = { goal : Bouquet
    , mode : UIMode }


bigFlower : Flower
bigFlower =
  Flower
    ( Garden
        [ Atom "a"
        , Flower
            ( Garden
                [ Atom "a" ] )
            [ Garden
                [ Atom "b" ],
              Garden
                [ Flower
                    ( Garden
                        [ Atom "b" ] )
                    [ Garden
                        [ Atom "c" ] ],
                  Atom "b" ] ]
        , Flower
            ( Garden
                [ Atom "d"] )
            [ Garden
                [ Atom "e" ] ] ] )
    [ Garden
        [ Atom "b"
        , Atom "a" ]
    , Garden
      [ Atom "c" ] ]


modusPonensCurryfied : Flower
modusPonensCurryfied =
  let
    ab =
      Flower
        ( Garden
            [ Atom "a" ] )
        [ Garden
            [ Atom "b" ] ]
  in
  Flower
    ( Garden [ ab ] ) 
    [ Garden [ ab ] ]


init : Model
init =
  { goal = [ bigFlower ]
  , mode = ProofMode Justifying }


-- UPDATE

type ProofRule
  = Justify -- down pollination
  | ImportStart -- up pollination (start drag-and-drop on source)
  | Import -- up pollination (stop drag-and-drop on destination)
  | ImportCancel -- up pollination (cancel drag-and-drop)
  | Unlock -- empty pistil
  | Close -- empty petal
  | Fence -- fencing


type Msg
  = ProofAction ProofRule Bouquet Zipper


update : Msg -> Model -> Model
update msg model =
  case msg of
    ProofAction rule bouquet zipper ->
      case (rule, bouquet, zipper) of
        (Justify, _, _) ->
          { model | goal = fillZipper [] zipper }
        
        (ImportStart, [source], _) ->
          { model | mode = ProofMode (Importing source) }
        
        (Import, _, _) ->
          case model.mode of
            ProofMode (Importing source) ->
              { model
              | goal = fillZipper (bouquet ++ [source]) zipper
              , mode = ProofMode Justifying }
            
            _ ->
              model
        
        (ImportCancel, _, _) ->
          { model | mode = ProofMode Justifying }

        (Unlock, [], (left, Pistil [Garden petal], right) :: parent)  ->
          { model | goal = fillZipper (left ++ petal ++ right) parent }
        
        (Unlock, [], (left, Pistil branches, right) :: (l, Pistil petals, r) :: parent) ->
          let
            case_ : Garden -> Flower
            case_ branch =
              Flower branch petals
            
            pistil =
              Garden (left ++ right)
            
            cases =
              List.map case_ branches  
          in
          { model
          | goal = fillZipper (l ++ (Flower pistil [Garden cases]) :: r) parent }
        
        (Close, [], (left, Petal _ _ _, right) :: parent) ->
          { model | goal = fillZipper (left ++ right) parent }

        _ ->
          model


-- VIEW


---- Text


viewFlowerText : Flower -> String
viewFlowerText flower =
  case flower of
    Atom name ->
      name    
    
    Flower pistil petals ->
      let
        pistilText =
          viewGardenText pistil

        petalsText =
          petals |>
          List.map viewGardenText |>
          String.join "; "
      in
      "(" ++ pistilText ++ " ⫐ " ++ petalsText ++ ")"


viewGardenText : Garden -> String
viewGardenText (Garden bouquet) =
  bouquet |>
  List.map viewFlowerText |>
  String.join ", "


viewZipperText : Zipper -> String
viewZipperText zipper =
  fillZipper [Atom "□"] zipper |>
  List.map (viewFlowerText) |>
  String.concat

logZipper : String -> Zipper -> String
logZipper msg zipper =
  zipper |>
  viewZipperText |>
  Debug.log msg

logBouquet : String -> Bouquet -> String
logBouquet msg bouquet =
  bouquet |>
  List.map viewFlowerText |>
  String.join ", " |>
  Debug.log msg


---- Graphics


transparent : Color
transparent =
  rgba 0 0 0 0


fgColor : Polarity -> Color
fgColor polarity =
  case polarity of
    Pos ->
      rgb 0 0 0
    Neg ->
      rgb 1 1 1

bgColor : Polarity -> Color
bgColor polarity =
  case polarity of
    Pos ->
      rgb 1 1 1
    Neg ->
      rgb 0 0 0


borderWidth : Int
borderWidth =
  3


actionable : List (Attribute Msg)
actionable =
  [ pointer
  , Border.width 3
  , Border.color (rgb 1.0 0.5 0)
  , Border.dotted ]


viewFlowerProof : ProofInteraction -> Context -> Flower -> Element Msg
viewFlowerProof interaction context flower =
  case flower of
    Atom name ->
      let
        justifyAction : List (Attribute Msg)
        justifyAction =
          if isHypothesis flower context.zipper then
            (Events.onClick (ProofAction Justify [flower] context.zipper))
            :: actionable
          else
            []
      in
      el
      [ width fill
      , height fill ]
        (el
          ( [ centerX
            , centerY
            , padding 3
            , Font.color (fgColor context.polarity)
            , Font.size 32 ]
           ++ justifyAction )
          ( text name ) )
    
    Flower pistil petals ->
      let
        (left, right, parent) =
          case context.zipper of
            (l, Hole, r) :: p ->
              (l, r, p)
            
            _ ->
              ([], [], [])

        pistilEl =
          let
            (Garden bouquet) = pistil

            newZipper =
              (left, Pistil petals, right) :: parent

            unlockAction =
              if List.isEmpty bouquet then
                (Events.onClick (ProofAction Unlock bouquet newZipper))
                :: actionable
              else
                []
          in
          el
            ( [ width fill
              , height fill
              , padding 20
              , Background.color (bgColor (negate context.polarity)) ]
             ++ unlockAction )
            ( viewGardenProof
                interaction
                { context
                | zipper = newZipper
                , polarity = negate context.polarity }
                pistil )
        
        petalsEl =
          let
            petalEl (leftPetals, rightPetals) petal =
              let
                (Garden bouquet) = petal

                newZipper =
                  (left, Petal pistil leftPetals rightPetals, right) :: parent

                explodeAction =
                  if List.isEmpty bouquet then
                    (Events.onClick (ProofAction Close bouquet newZipper))
                    :: actionable
                  else
                    []
              in
              el
                ( [ width fill
                  , height fill
                  , padding 20
                  , Background.color (bgColor context.polarity) ]
                 ++ explodeAction )
                ( viewGardenProof
                    interaction
                    { context
                    | zipper = newZipper }
                    petal )
          in
          row
            [ width fill
            , height fill
            , spacing borderWidth ]
            (Utils.List.zipMap petalEl petals)  

        importStartAction =
          [onMouseDown (ProofAction ImportStart [flower] context.zipper)]
      in
      column
        ( [ width fill
          , height fill
          , Background.color (bgColor (negate context.polarity))
          , Border.color (bgColor (negate context.polarity))
          , Border.width borderWidth ]
         ++ importStartAction )
        [ pistilEl, petalsEl ]


viewGardenProof : ProofInteraction -> Context -> Garden -> Element Msg
viewGardenProof interaction context (Garden bouquet) =
  let
    flowerEl (left, right) =
      viewFlowerProof
        interaction
        { context
        | zipper = (left, Hole, right) :: context.zipper }
    
    importAction =
      case interaction of
        Importing content ->
          if isHypothesis content context.zipper then
            [ onMouseUp (ProofAction Import bouquet context.zipper)
            , mouseOver [ Border.color (rgb 1 0.8 0) ] ]
          else
            [ onMouseUp (ProofAction ImportCancel bouquet context.zipper) ]

        _ ->
          []
  in
  wrappedRow
    ( [ width fill
      , height fill
      , spacing 40
      , Border.width 3
      , Border.dashed
      , Border.color transparent ]
     ++ importAction )
    (Utils.List.zipMap flowerEl bouquet)


view : Model -> Element Msg
view model =
  -- text (viewFlowerText model)
  let
    bouquetEls =
      case model.mode of
        ProofMode interaction ->
          (Utils.List.zipMap
            (\(l, r) -> viewFlowerProof interaction (Context [(l, Hole, r)] Pos))
            model.goal)

        EditMode ->
          Debug.todo ""
  in
  column
    [ width fill
    , height fill
    , spacing 100
    , Background.color (rgb 0.65 0.65 0.65) ]
    bouquetEls
