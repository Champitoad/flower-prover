module View.Widgets exposing (..)

import Element exposing (..)

import Css
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Events
import Html.Styled.Attributes exposing (title, css)

import FeatherIcons as Icons
import FeatherIcons exposing (Icon)


buttonBorderRadius : number
buttonBorderRadius =
  10


buttonHeight : number
buttonHeight =
  55


button : msg -> String ->  Icon -> Bool -> Element msg
button msg name icon enabled =
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
    ( css style :: title name :: action )
    [ iconStyledHtml ]
  |> toUnstyled
  |> html