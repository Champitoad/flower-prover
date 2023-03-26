module View.Toolbar exposing (..)

import View.Style exposing (..)

import Model.App exposing (..)

import Update.App exposing (..)

import Utils.Color
import Utils.Maybe exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import Html.Attributes exposing (title)

import Css
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Events
import Html.Styled.Attributes exposing (css)

import FeatherIcons as Icons
import FeatherIcons exposing (Icon)


type ModeSelectorPosition
  = Start | Middle | End


buttonBorderRadius : number
buttonBorderRadius =
  10


buttonHeight : number
buttonHeight =
  55


button : msg -> Icon -> Bool -> Element msg
button msg icon enabled =
  let
    iconStyledHtml =
      let
        iconColor =
          if enabled then
            Css.rgb 0 0 0
          else
            Css.rgb 127 127 127
        iconHtml =
          icon
          |> Icons.withSize 30
          |> Icons.toHtml []
          |> fromUnstyled
      in
      Html.Styled.div
        [ css [ Css.color iconColor ] ]
        [ iconHtml ]
    
    style =
      Css.width (Css.px buttonHeight) ::
      Css.height (Css.px buttonHeight) ::

      Css.borderStyle Css.solid ::
      Css.borderRadius (Css.px buttonBorderRadius) ::
      Css.borderWidth (Css.px 1) ::

      Css.displayFlex ::
      Css.alignItems Css.center ::
      Css.justifyContent Css.center ::

      if enabled then
        [ Css.cursor Css.pointer
        , Css.borderColor (Css.rgb 180 180 180)
        , Css.backgroundColor (Css.rgb 240 240 240)
        , Css.hover [Css.backgroundColor (Css.rgb 250 250 250)]
        , Css.active [Css.backgroundColor (Css.rgb 180 180 180)] ]
      else
        [ Css.borderColor Css.transparent ]
    
    action =
      if enabled then [Html.Styled.Events.onClick msg] else []
  in
  Html.Styled.div
    ( css style :: action )
    [ iconStyledHtml ]
  |> toUnstyled
  |> html


viewAutoButton : UIMode -> Element Msg
viewAutoButton mode =
  let
    enabled =
      case mode of
        ProofMode _ -> True
        _ -> False
  in
  button Auto Icons.settings enabled


viewModeSelector : UIMode -> Element Msg
viewModeSelector currentMode =
  let
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
                  |> Icons.toHtml [ fgColor 
                                    |> Utils.Color.fromElement
                                    |> Utils.Color.toHtml ]
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
          , height (buttonHeight |> px)
          , Background.color bgColor
          , Border.roundEach borderRound
          , htmlAttribute <| title titleText ]
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
    , item (EditMode Erasing) Middle
    , item NavigationMode End ]


viewUndoRedo : History -> Element Msg
viewUndoRedo (History history) =
  let
    (undoEnabled, redoEnabled) =
      Tuple.mapBoth isSomething isSomething (history.prev, history.next)
  in
  row []
    [ button Undo Icons.arrowLeft undoEnabled
    , button Redo Icons.arrowRight redoEnabled ]


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