module Test.PreventPropagation exposing (..)

import Browser

import Html
import Html.Events
import Json.Decode as Json

import Element exposing (..)
import Element.Background as Background
import Element.Events as Events

import Utils.Events exposing (..)


-- MAIN


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


-- Using behindContent does not work properly with centering


testCentering1 : Bool -> Element Msg
testCentering1 doCentering =
  let
    centering =
      if doCentering then
        [centerX, centerY]
      else
        []
  in
  row
    [ width fill
    , height (200 |> px)
    , Background.color (rgb 1 0 0)
    , behindContent
        ( el
            [ Events.onClick Hello
            , width fill
            , height fill ]
            none ) ]
    [ el
        ( [ width (100 |> px)
          , height (100 |> px)
          , Background.color (rgb 0 0 1)
          , behindContent
              ( el
                  [ Events.onClick World
                  , width fill
                  , height fill ]
                  none ) ]
          ++ centering )
        none ]


view1 : Model -> Element Msg
view1 model =
  column
    [ width fill
    , height fill
    , spacing 10 ]
    [ el  
        [ width fill
        , height (100 |> px) ]
        ( el [centerX, centerY] (text model) )
    , testCentering1 False
    , testCentering1 True]


-- With Html.Events.stopPropagationOn it works!


testCentering2 : Bool -> Element Msg
testCentering2 doCentering =
  let
    centering =
      if doCentering then
        [centerX, centerY]
      else
        []
  in
  row
    [ width fill
    , height (200 |> px)
    , Background.color (rgb 1 0 0)
    , Events.onMouseDown Hello ]
    [ el
        ( [ width (100 |> px)
          , height (100 |> px)
          , Background.color (rgb 0 0 1)
          , onMouseDown World ]
        ++ centering )
      none ]


view2 : Model -> Element Msg
view2 model =
  column
    [ width fill
    , height fill
    , spacing 10 ]
    [ el  
        [ width fill
        , height (100 |> px) ]
        ( el [centerX, centerY] (text model) )
    , testCentering2 False
    , testCentering2 True]


view : Model -> Element Msg
view =
  view2