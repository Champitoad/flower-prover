module View.Toolbar exposing (..)

import View.Style exposing (..)
import View.Widgets exposing (..)

import Model.App exposing (..)

import Update.App exposing (..)

import Utils.Color
import Utils.Maybe exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import Html.Attributes

import FeatherIcons as Icons


type ModeSelectorPosition
  = Start | Middle | End


viewAutoButton : UIMode -> Element Msg
viewAutoButton mode =
  let
    enabled =
      case mode of
        ProofMode _ -> True
        _ -> False
  in
  defaultButton
    { msg = Auto
    , title = "Auto"
    , icon = Icons.settings
    , enabled = enabled }


viewModeSelector : UIMode -> Element Msg
viewModeSelector currentMode =
  let
    item mode position =
      let
        isSelected =
          case (mode, currentMode) of          
            (ProofMode _, ProofMode _) -> True
            (EditMode _ _, EditMode _ _) -> True
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
                EditMode _ _ -> ("Edit", Icons.edit2)
                NavigationMode -> ("Navigate", Icons.navigation)
            elem =
              el
                [ centerX, centerY ]
                ( icon
                  |> Icons.withSize 30
                  |> Icons.toHtml [ fgColor 
                                    |> Utils.Color.fromElement
                                    |> Utils.Color.toHtmlAttr ]
                  |> html )
          in
          (elem, title)
        
        borderRound =
          case position of
            Start ->
              { topLeft = buttonBorderRadius, bottomLeft = buttonBorderRadius
              , topRight = 0, bottomRight = 0 }
            Middle ->
              { topLeft = 0, bottomLeft = 0
              , topRight = 0, bottomRight = 0 }
            End ->
              { topLeft = 0, bottomLeft = 0
              , topRight = buttonBorderRadius, bottomRight = buttonBorderRadius }

        changeAction =
          [ Events.onClick (ChangeUIMode mode)
          , pointer ]
      in
      el
        ( [ width (60 |> px)
          , height (defaultButtonSize |> px)
          , Background.color bgColor
          , Border.roundEach borderRound
          , htmlAttribute <| Html.Attributes.title titleText ]
         ++ changeAction )
        iconEl
    
    borderColor = rgb 0.6 0.6 0.6
  in
  row
    [ width shrink
    , height shrink
    , spacing 1
    , Border.width 0
    , Border.rounded buttonBorderRadius
    , Border.color borderColor
    , Background.color borderColor ]
    [ item (ProofMode Justifying) Start
    , item (EditMode Operating initialSurgery) Middle
    , item NavigationMode End ]


viewUndoRedo : History -> Element Msg
viewUndoRedo (History history) =
  let
    (undoEnabled, redoEnabled) =
      Tuple.mapBoth isSomething isSomething (history.prev, history.next)
  in
  row []
    [ defaultButton
        { msg = Undo
        , title = "Undo"
        , icon = Icons.arrowLeft
        , enabled = undoEnabled }
    , defaultButton
        { msg = Redo
        , title = "Redo"
        , icon = Icons.arrowRight
        , enabled = redoEnabled } ]


viewToolbar : Model -> Element Msg
viewToolbar model =
  let
    autoButton = viewAutoButton model.mode
    modeSelector = viewModeSelector model.mode
    undoRedo = viewUndoRedo model.history
  in
  row
    [ width fill
    , height shrink
    , padding 15
    , Background.gradient
        { angle = 0
        , steps = [ rgb 0.8 0.8 0.8, rgb 0.9 0.9 0.9 ] } ]
    [ el
        [ width fill ]
        ( el
            [ alignLeft ]
            ( autoButton ) )
    , modeSelector
    , el
        [ width fill ]
        ( el
            [ alignRight ]
            ( undoRedo ) ) ]