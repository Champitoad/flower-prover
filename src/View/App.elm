module View.App exposing (..)

import View.Goal exposing (..)
import View.Toolbar exposing (..)
import View.Style exposing (fillXY)

import Model.App exposing (Model)

import Update.App exposing (..)

import Element exposing (..)

import Html exposing (Html, div)
import Html.Events exposing (on)
import Html.Attributes exposing (style)

import Json.Decode

import Keyboard.Event exposing (decodeKeyboardEvent)
import Html.Attributes exposing (tabindex)


keyboardListener : Html.Attribute Msg
keyboardListener =
  on "keydown"
  <| Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent


view : Model -> Html Msg
view model =
  let
    goal =
      viewGoal model
    
    toolbar =
      viewToolbar model
    
    app =
      column fillXY [ goal, toolbar ]
      |> layout []
  in
  div
    [ style "width" "100%"
    , style "height" "100%"
    , keyboardListener
    , tabindex 0 ]
    [ app ]