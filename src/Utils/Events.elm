module Utils.Events exposing (..)

import Html.Events
import Json.Decode as Json

import Element exposing (..)


onClick : msg -> Attribute msg
onClick =
  alwaysStopPropagationOn "onclick"


onMouseDown : msg -> Attribute msg
onMouseDown =
  alwaysStopPropagationOn "mousedown"


onMouseUp : msg -> Attribute msg
onMouseUp =
  alwaysStopPropagationOn "mouseup"


onMouseMove : msg -> Attribute msg
onMouseMove =
  alwaysStopPropagationOn "mousemove"


alwaysStopPropagationOn : String -> msg -> Attribute msg
alwaysStopPropagationOn event msg =
  let always m = ( m, True ) in
  Html.Events.stopPropagationOn event (Json.map always (Json.succeed msg)) |>
  htmlAttribute
