module View.Manual exposing (..)

import View.Style exposing (..)
import View.Goal exposing (..)
import Model.Goal exposing (..)
import Model.App exposing (..)
import Update.App exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input

import FeatherIcons as Icons


resetIcon : Element msg
resetIcon =
  Icons.refreshCw |>
  Icons.withSize 20 |>
  Icons.toHtml [] |>
  html


resetButton : SandboxID -> Element Msg
resetButton id =
  Input.button
    [ ]
    { onPress = Just (ResetSandbox id)
    , label = resetIcon
    }

viewSandbox : FlowerDnD -> Sandbox -> String -> Element Msg
viewSandbox dnd { currentGoal } id =
  row
    [ width fill
    , spacing 10
    ]
    [ viewGoal dnd currentGoal
    , resetButton id
    ]
    

backButtonStyle : List (Attribute msg)
backButtonStyle =
  [ Border.width 2
  , Border.rounded 4
  , Border.solid
  , padding 10 ]


navbar : Element msg
navbar =
  row
    [ width fill
    , padding 20 ]
    [ link
        ( alignLeft :: backButtonStyle )
        { url = "/", label = text "back to app" } ]


body : Model -> Element Msg
body { manualExamples, dragDrop } =
  let
    sandbox id =
      viewSandbox dragDrop (getSandbox id manualExamples) id
    
    padder =
      el [width shrink, height fill] none
  in
  row
    ( scrollbarY ::
      fillXY )
    [ padder
    , column
      [ width (fill |> maximum 600)
      , height fill
      , padding 20
      , spacing 20
      , centerX ]
      [ text "Look at this box:"
      , sandbox "Flower"
      , sandbox "Justify"
      , sandbox "Modus Ponens"
      ]
    , padder
    ]


page : Model -> Element Msg
page model =
  column
    [ width fill
    , height fill ]
    [ body model
    , navbar ]