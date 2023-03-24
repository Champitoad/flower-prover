module View.Toolbar exposing (..)

import View.Style exposing (..)

import Model.App exposing (..)

import Update.App exposing (..)

import Utils.Color as Color
import Utils.Maybe exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input

import Html.Attributes exposing (title)

import FeatherIcons as Icons
import FeatherIcons exposing (Icon)


type ModeSelectorPosition
  = Start | Middle | End


borderRoundSize : Int
borderRoundSize =
  10


buttonHeight : Length
buttonHeight =
  55 |> px


button : msg -> Icon -> Bool -> Element msg
button msg icon enabled =
  let
    iconColor =
      if enabled then
        rgb 0 0 0
      else
        rgb 0.5 0.5 0.5

    iconEl =
      icon
      |> Icons.withSize 30
      |> Icons.toHtml [ iconColor |> Color.fromElement |> Color.toHtml ]
      |> html
    
    enabledStyle =
      [ Background.color (rgb 0.925 0.925 0.925)
      , mouseOver [Background.color (rgb 1 1 1)]
      , Border.rounded borderRoundSize
      , Border.color (rgb 0.6 0.6 0.6)
      , Border.width 1 ]
    
    disabledStyle =
      [focused []]
  in
  Input.button
    ( [ width buttonHeight
      , height buttonHeight ]
     ++ if enabled then enabledStyle else disabledStyle )
    { onPress = if enabled then Just msg else Nothing
    , label = centered iconEl }


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
          , height buttonHeight
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
    , Border.rounded borderRoundSize
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
    modeSelector = viewModeSelector model.mode
    undoRedo = viewUndoRedo
  in
  row
    [ width fill
    , height shrink
    , padding 15
    , Background.gradient
        { angle = 0
        , steps = [ rgb 0.8 0.8 0.8, rgb 0.9 0.9 0.9 ] } ]
    [ el [ width fill ] none
    , modeSelector
    , el
        [ width fill ]
        ( el
            [ alignRight ]
            ( undoRedo model.history ) ) ]