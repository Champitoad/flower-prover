module Utils.Style exposing (..)

import Element
import Html
import Html.Attributes exposing (style)
import Color

htmlColor : Element.Color -> Html.Attribute msg
htmlColor color =
  let value = color |> Element.toRgb |> Color.fromRgba |> Color.toCssString in
  style "color" value