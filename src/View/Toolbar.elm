module View.Toolbar exposing (..)

import Model.App exposing (..)

import Update.App exposing (..)

import Utils.Color as Color

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import Html.Attributes exposing (title)

import FeatherIcons as Icons


type ModeSelectorPosition
  = Start | Middle | End


viewModeSelector : UIMode -> Element Msg
viewModeSelector currentMode =
  let
    borderRoundSize = 10

    item mode position =
      let
        isSelected =
          case (mode, currentMode) of          
            (ProofMode _, ProofMode _) -> True
            (EditMode _, EditMode _) -> True
            _ -> mode == currentMode

        (bgColor, fgColor) =
          if isSelected
          then (rgb255 58 134 255, rgb 1 1 1)
          else (rgb 1 1 1, rgb 0 0 0)

        (iconEl, titleText) =
          let
            (title, icon) =
              case mode of
                ProofMode _ -> ("Prove", Icons.checkSquare)
                EditMode _ -> ("Edit", Icons.edit2)
                NavigationMode -> ("Navigate", Icons.navigation)
            elem =
              el
                [ centerX, centerY ]
                ( icon
                  |> Icons.withSize 30
                  |> Icons.toHtml [ fgColor |> Color.fromElement |> Color.toHtml ]
                  |> html )
          in
          (elem, title)
        
        borderRound =
          case position of
            Start ->
              { topLeft = borderRoundSize, bottomLeft = borderRoundSize
              , topRight = 0, bottomRight = 0 }
            Middle ->
              { topLeft = 0, bottomLeft = 0
              , topRight = 0, bottomRight = 0 }
            End ->
              { topLeft = 0, bottomLeft = 0
              , topRight = borderRoundSize, bottomRight = borderRoundSize }

        changeAction =
          [ Events.onClick (ChangeUIMode mode)
          , pointer ]
      in
      el
        ( [ width (60 |> px)
          , height (55 |> px)
          , Background.color bgColor
          , Border.roundEach borderRound
          , htmlAttribute <| title titleText ]
         ++ changeAction )
        iconEl
    
    borderColor = rgb 0.6 0.6 0.6
  in
  row
    [ width shrink
    , height fill
    , centerX, centerY
    , spacing 1
    , Border.width 0
    , Border.rounded borderRoundSize
    , Border.color borderColor
    , Background.color borderColor ]
    [ item (ProofMode Justifying) Start
    , item (EditMode Erasing) Middle
    , item NavigationMode End ]


viewUndoRedo : Element Msg
viewUndoRedo =
  let
    undoAction =
      [Events.onClick Undo]
  in
  row
    [ width shrink
    , height shrink ]
    [ el
        ( [ width (60 |> px)
          , height (60 |> px) ]
         ++ undoAction )
        ( Icons.arrowLeftCircle
          |> Icons.withSize 30
          |> Icons.toHtml []
          |> html ) ]


viewToolbar : Model -> Element Msg
viewToolbar model =
  row
    [ width fill
    , height shrink
    , padding 15
    , Background.color (rgb 0.9 0.9 0.9) ]
    [ viewModeSelector model.mode
    , viewUndoRedo ]