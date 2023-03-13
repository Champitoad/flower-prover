module Main exposing (..)

import Utils.List

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events


-- MAIN


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

    Petal pistil left right ->
      [Flower pistil (left ++ Garden bouquet :: right)]


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


type Polarity
  = Pos
  | Neg


negate : Polarity -> Polarity
negate polarity =
  case polarity of
    Pos -> Neg
    Neg -> Pos


type alias Context =
  { zipper : Zipper,
    polarity : Polarity }
 

type alias Model
  = Bouquet


init : Model
init =
  [ Flower
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
          , Atom "d" ] )
      [ Garden
          [ Atom "b"
          , Atom "a" ]
      , Garden
        [ Atom "c" ] ] ]


-- UPDATE


type Msg
  = Justify Zipper Flower


update : Msg -> Model -> Model
update msg model =
  case msg of
    Justify zipper flower ->
      if List.member flower (hypsZipper zipper) then
        fillZipper [] zipper
      else
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
      pistilText ++ " ⫐ " ++ petalsText


viewGardenText : Garden -> String
viewGardenText (Garden bouquet) =
  bouquet |>
  List.map viewFlowerText |>
  String.join ", "


---- Graphics


fgColor : Polarity -> Color
fgColor polarity =
  case polarity of
    Pos ->
      rgb 0 0 0
    Neg ->
      rgb 255 255 255

bgColor : Polarity -> Color
bgColor polarity =
  case polarity of
    Pos ->
      rgb 255 255 255
    Neg ->
      rgb 0 0 0


borderWidth : Int
borderWidth =
  3


viewFlower : Context -> Flower -> Element Msg
viewFlower context flower =
  case flower of
    Atom name ->
      el
      [ width fill
      , height fill ]
        (el
          [ centerX
          , centerY
          , padding 3
          -- , Border.width 1
          , Border.dashed
          , Border.color (fgColor context.polarity)
          , Font.color (fgColor context.polarity)
          , Font.size 32
          , Events.onClick (Justify context.zipper flower) ]
          (text name))
    
    Flower pistil petals ->
      let
        pistilEl =
          viewGarden
            { context
            | zipper = Pistil petals :: context.zipper
            , polarity = negate context.polarity }
            pistil
        
        petalsEl =
          let
            petalEl (left, right) petal =
              viewGarden
                { context
                | zipper = Petal pistil left right :: context.zipper }
                petal
          in
          row
            [ width fill
            , height fill
            , spacing borderWidth ]
            (Utils.List.zipMap petalEl petals)
      in
      column
        [ width fill
        , height fill
        , Background.color (bgColor (negate context.polarity))
        , Border.color (bgColor (negate context.polarity))
        , Border.width borderWidth ]
        [ pistilEl, petalsEl ]


viewGarden : Context -> Garden -> Element Msg
viewGarden context (Garden bouquet) =
  let
    flowerEl (left, right) =
      viewFlower
        { context
        | zipper = Bouquet left right :: context.zipper }
  in
  wrappedRow
    [ width fill
    , height fill
    , padding 20
    , spacing 40
    , Background.color (bgColor context.polarity) ]
    (Utils.List.zipMap flowerEl bouquet)


view : Model -> Element Msg
view model =
  -- text (viewFlowerText model)
  column [width fill, height fill]
    (List.map (viewFlower (Context [] Pos)) model)
