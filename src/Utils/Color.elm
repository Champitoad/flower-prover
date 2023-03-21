module Utils.Color exposing (..)

import Element
import Html
import Html.Attributes exposing (style)
import Color exposing (..)


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


type alias Color = Color.Color


toElement : Color -> Element.Color
toElement color =
  color |> Color.toRgba |> Element.fromRgb


fromElement : Element.Color -> Color
fromElement color =
  color |> Element.toRgb |> Color.fromRgba


toHtml : Color -> Html.Attribute msg
toHtml color =
  let value = color |> Color.toCssString in
  style "color" value


withAlpha : Float -> Color -> Color
withAlpha alpha color =
  let rgba = Color.toRgba color in
  Color.fromRgba { rgba | alpha = alpha }


transparent : Color
transparent =
  Color.fromRgba { red = 1, green = 1, blue = 1, alpha = 1 }