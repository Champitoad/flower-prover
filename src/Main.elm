module Main exposing (..)

import Element exposing (Element, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


-- MAIN


main = 
  Element.layout []
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
  Flower (Garden [Atom "a"]) [Garden [Atom "a", Atom "b"], Garden [Atom "c"]]


-- UPDATE


-- VIEW


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


view : Model -> Element msg
view model =
  Element.text (viewFlowerText model)
