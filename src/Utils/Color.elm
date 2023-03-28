module Utils.Color exposing (..)

import Element

import Html
import Html.Attributes exposing (style)

import Css

import Color exposing (..)


type alias Color = Color.Color


-- Color model conversions


fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba =
  Color.fromRgba


toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba =
  Color.toRgba


fromRgb : { red : Float, green : Float, blue : Float } -> Color
fromRgb { red, green, blue } =
  Color.fromRgba { red = red, green = green, blue = blue, alpha = 1 }


toRgb : Color -> { red : Float, green : Float, blue : Float }
toRgb color =
  let { red, green, blue } = Color.toRgba color in
  { red = red, green = green, blue = blue }


toRgba255 : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
toRgba255 color =
  let { red, green, blue, alpha } = toRgba color in
  let int f = round (f * 255) in
  { red = int red
  , green = int green
  , blue = int blue
  , alpha = alpha }


-- Library conversions


toElement : Color -> Element.Color
toElement color =
  color |> Color.toRgba |> Element.fromRgb


fromElement : Element.Color -> Color
fromElement color =
  color |> Element.toRgb |> Color.fromRgba


toHtmlAttr : Color -> Html.Attribute msg
toHtmlAttr color =
  let value = color |> Color.toCssString in
  style "color" value


toCss : Color -> Css.Color
toCss color =
  let { red, green, blue, alpha } = toRgba255 color in
  Css.rgba red green blue alpha


-- Utility functions


transparent : Color
transparent =
  Color.fromRgba { red = 1, green = 1, blue = 1, alpha = 1 }


withAlpha : Float -> Color -> Color
withAlpha alpha color =
  let rgba = Color.toRgba color in
  Color.fromRgba { rgba | alpha = alpha }


changeLight : Float -> Color -> Color
changeLight factor color =
  let ({ lightness } as hsla) = Color.toHsla color in
  Color.fromHsla { hsla | lightness = factor * lightness }