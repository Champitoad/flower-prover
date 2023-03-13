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


---- TEXT


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


type Polarity
  = Pos
  | Neg

negate : Polarity -> Polarity
negate polarity =
  case polarity of
    Pos -> Neg
    Neg -> Pos


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


viewFlower : Polarity -> Flower -> Element msg
viewFlower polarity flower =
  case flower of
    Atom name ->
      el [ width fill ]
        (el
          [ centerX
          , Font.color (fgColor polarity)
          , Font.size 32 ]
          (text name))
    
    Flower pistil petals ->
      let
        pistilEl =
          viewGarden (negate polarity) pistil
        
        petalsEl =
          let
            petalEl petal =
              (viewGarden polarity petal)
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
        , Background.color (bgColor (negate polarity))
        , Border.color (bgColor (negate polarity))
        , Border.width borderWidth ]
        [ pistilEl, petalsEl ]


viewGarden : Polarity -> Garden -> Element msg
viewGarden polarity (Garden bouquet) =
  wrappedRow
    [ width fill
    , height fill
    , padding 20
    , spacing 40
    , Background.color (bgColor polarity) ]
    (List.map (viewFlower polarity) bouquet)


view : Model -> Element msg
view model =
  -- text (viewFlowerText model)
  viewFlower Pos model
