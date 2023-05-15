module Test.DragAndDrop exposing (..)

import Browser

import Element exposing (..)
import Element.Background as Background


-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = \model -> layout [] (view model) }


-- MODEL


type alias Model =
  String


init : Model
init =
  ""


-- UPDATE


type Msg
  = Hello
  | World


update : Msg -> Model -> Model
update msg _ =
  case msg of
    Hello ->
      "Hello"

    World ->
      "World"


-- VIEW


block : List (Attribute Msg) -> Element Msg
block attrs =
  el
    ( [ width fill
      ,  height (100 |> px) ]
      ++ attrs )
    none


view : Model -> Element Msg
view model =
  column
    [ width fill
    , height fill
    , spacing 100 ]
    [ el  
        [ width fill
        , height (100 |> px) ]
        ( el [centerX, centerY] (text model) )
    , block
        [ Background.color (rgb 1 0 0) ]
    , block
        [ Background.color (rgb 0 1 0) ] ]