module Utils.Events exposing (onMouseDown, onMouseUp, alwaysStopPropagationOn)

import Html.Events
import Json.Decode as Json

import Element exposing (..)


onMouseDown : msg -> Attribute msg
onMouseDown =
  alwaysStopPropagationOn "mousedown"


onMouseUp : msg -> Attribute msg
onMouseUp =
  alwaysStopPropagationOn "mouseup"


alwaysStopPropagationOn : String -> msg -> Attribute msg
alwaysStopPropagationOn event msg =
  Html.Events.stopPropagationOn event (Json.map always (Json.succeed msg)) |>
  htmlAttribute


always : msg -> ( msg, Bool )
always msg =
  ( msg, True )