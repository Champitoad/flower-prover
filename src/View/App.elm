module View.App exposing (..)

import View.Goal exposing (..)
import View.Toolbar exposing (..)
import View.Style exposing (fillXY)

import Model.App exposing (Model)

import Update.App exposing (..)

import Element exposing (..)

import Html exposing (div)
import Html.Events exposing (on)
import Html.Attributes exposing (style)

import Json.Decode

import Keyboard.Event exposing (decodeKeyboardEvent)
import Html.Attributes exposing (tabindex)
import Browser exposing (Document)

import View.Route as Route
import View.Widgets as Widgets
import View.Manual as Manual
import Model.Flower exposing (logBouquet)


keyboardListener : Html.Attribute Msg
keyboardListener =
  on "keydown"
  <| Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent


view : Model -> Document Msg
view model =
  case Route.fromUrl model.url of
    Route.App ->
      let
        goal =
          viewGoal model.goal
        
        toolbar =
          viewToolbar model
        
        app =
          column fillXY [ goal, toolbar ]
          |> layout []
      in
      { title = "Flower Prover"
      , body =
        [
          div
          [ style "width" "100%"
          , style "height" "100%"
          , keyboardListener
          , tabindex 0 ]
          [ app ]
        ]
      }
    
    Route.Manual ->
      { title = "Manual · Flower Prover"
      , body = [ layout [] Manual.page ]
      }
    
    Route.NotFound _ ->
      { title = "Error 404"
      , body = [ layout [] (Widgets.fullPageTextMessage "Page not found!") ]
      }