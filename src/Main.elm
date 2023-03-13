module Main exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


-- MAIN


main = 
  layout []
    (view init)


-- MODEL


type Flower
  = Atom String
  | Flower Garden (List Garden)


type Garden
  = Garden (List Flower)


type Zipper
  = Hole
  | Bouquet (List Flower) Zipper (List Flower)
  | Pistil Zipper (List Garden)
  | Petal Garden (List Garden) Zipper (List Garden)


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
  = Flower


init : Model
init =
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
        , Atom "d" ] )
    [ Garden
        [ Atom "b"
        , Atom "a" ]
    , Garden
      [ Atom "c" ] ]


-- UPDATE


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


viewFlower : Context -> Flower -> Element msg
viewFlower context flower =
  case flower of
    Atom name ->
      el [ width fill ]
        (el
          [ centerX
          , Font.color (fgColor context.polarity)
          , Font.size 32 ]
          (text name))
    
    Flower pistil petals ->
      let
        pistilEl =
          viewGarden ({ context | polarity = negate context.polarity }) pistil
        
        petalsEl =
          let
            petalEl petal =
              (viewGarden context petal)
          in
          row
            [ width fill
            , height fill
            , spacing borderWidth ]
            (List.map petalEl petals)
      in
      column
        [ width fill
        , height fill
        , Background.color (bgColor (negate context.polarity))
        , Border.color (bgColor (negate context.polarity))
        , Border.width borderWidth ]
        [ pistilEl, petalsEl ]


viewGarden : Context -> Garden -> Element msg
viewGarden context (Garden bouquet) =
  wrappedRow
    [ width fill
    , height fill
    , padding 20
    , spacing 40
    , Background.color (bgColor context.polarity) ]
    (List.map (viewFlower context) bouquet)


view : Model -> Element msg
view model =
  -- text (viewFlowerText model)
  viewFlower (Context Hole Pos) model
