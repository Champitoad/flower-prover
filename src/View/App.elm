module View.App exposing (..)

import View.Goal exposing (..)
import View.Toolbar exposing (..)

import Model.App exposing (Model)

import Update.App exposing (Msg)

import Element exposing (..)

import Html exposing (Html)


view : Model -> Html Msg
view model =
  let
    goal =
      viewGoal model
    
    toolbar =
      viewToolbar model
    
    app =
      column
        [ width fill,
          height fill ]
        [ goal, toolbar ]
  in
  layout [] app