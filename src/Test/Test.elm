module Test.Test exposing (..)

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events


-- MAIN


main =
  layout [] (view model)


-- MODEL


type alias Model =
  ()


model : Model
model =
  ()


-- UPDATE


-- VIEW


block : List (Attribute msg) -> Color -> String -> Element msg
block attrs color str =
  el
   ([ width fill
    , height fill
    , Background.color color ] ++ attrs)
    ( el 
        [ centerX
        , centerY ]
        (text str) )


view : Model -> Element msg
view _ =
  column
    [ width fill
    , height (100 |> px |> maximum 100) ]
    [ block [height (fill |> minimum 80)] (rgb 1 0.6 0) "hello"
    , block [height (fill |> minimum 80)] (rgb 0.6 1 0) "world" ]