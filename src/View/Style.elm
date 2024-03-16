module View.Style exposing (..)

import Model.Flower exposing (..)
import Model.App exposing (..)

import Utils.Color as Color

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

import Html.Attributes exposing (style)


-- Interaction


nonSelectable : Attribute msg
nonSelectable =
  htmlAttribute <| style "user-select" "none"


-- Text


bold : String -> Element msg
bold txt =
  el [Font.bold]
    (text txt)


italic : String -> Element msg
italic txt =
  el [Font.italic]
    (text txt)


-- Layout


fillXY : List (Attribute msg)
fillXY =
  [width fill, height fill]


centered : Element msg -> Element msg
centered elem =
  el [centerX, centerY] elem


-- Color


transparent : Color
transparent =
  rgba 0 0 0 0


-- Flower styling


flowerForegroundColor : Polarity -> Color
flowerForegroundColor polarity =
  case polarity of
    Pos ->
      rgb 0 0 0
    Neg ->
      rgb 1 1 1

flowerBackgroundColor : Polarity -> Color
flowerBackgroundColor polarity =
  case polarity of
    Pos ->
      rgb 1 1 1
    Neg ->
      rgb 0 0 0


grownColor : Color
grownColor =
  rgb 0.227 0.525 1


flowerBorderWidth : Int
flowerBorderWidth =
  3

flowerBorderRound : Int
flowerBorderRound =
  10


type alias ZoneStyle msg
  = { borderWidth : Int
    , active : List (Attribute msg)
    , inactive : List (Attribute msg) }


actionable : Color.Color -> ZoneStyle msg
actionable color =
  let
    width =
      5

    border =
      [ Border.width width
      , Border.dotted
      , Border.rounded flowerBorderRound ]
    
    bgColor =
      Color.withAlpha 0.5 color |> Color.toElement
  in
  { borderWidth = width
  , active =
      pointer ::
      Border.color (Color.toElement color) ::
      Background.color bgColor ::
      border
  , inactive =
      Border.color transparent ::
      border }


greenActionable : ZoneStyle msg
greenActionable =
  actionable (Color.fromRgb { red = 0.3, green = 0.9, blue = 0.3 })

pinkActionable : ZoneStyle msg
pinkActionable =  
  actionable (Color.fromRgb { red = 1, green = 0.1, blue = 0.8 })

orangeActionable : ZoneStyle msg
orangeActionable =
  actionable (Color.fromRgb { red = 1, green = 0.6, blue = 0 })

redActionable : ZoneStyle msg
redActionable =
  actionable (Color.fromRgb { red = 1, green = 0, blue = 0 })


draggable : Color.Color -> ZoneStyle msg
draggable color =
  let
    width =
      3

    border =
      [ Border.width width
      , Border.solid
      , Border.rounded flowerBorderRound ]
    
    borderColor =
      color |> Color.toElement
  in
  { borderWidth = width
  , active = Border.color borderColor :: border
  , inactive = Border.color transparent :: border }


droppable : Color.Color -> ZoneStyle msg
droppable color =
  let
    width =
      3

    border =
      [ Border.width width
      , Border.dashed
      , Border.rounded flowerBorderRound
      , Border.color (Color.toElement color) ]
    
    bgColor =
      Color.withAlpha 0.5 color |> Color.toElement
  in
  { borderWidth = width
  , active = Background.color bgColor :: border
  , inactive = border }


grownBorder : ZoneStyle msg
grownBorder =
  let
    width =
      3

    border =
      [ Border.rounded flowerBorderRound ]
  in
  { borderWidth = width
  , active =
      Border.color grownColor ::
      Border.solid ::
      Border.width width ::
      border
  , inactive =
      border }
