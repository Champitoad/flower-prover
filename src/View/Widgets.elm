module View.Widgets exposing (..)

import Utils.Color
import Utils.Events

import Element exposing (..)

import Css
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Attributes as Attrs exposing (css)

import FeatherIcons as Icons
import FeatherIcons exposing (Icon)

import Color


buttonBorderRadius : number
buttonBorderRadius =
  10


defaultButtonSize : number
defaultButtonSize =
  55


type alias ButtonStyle widthUnit heightUnit
  = { width : Css.LengthOrAuto widthUnit
    , height : Css.LengthOrAuto heightUnit
    , color : Color.Color
    , iconColorEnabled : Color.Color
    , iconColorDisabled : Color.Color }


type alias ButtonParams msg
  = { msg : msg
    , title : String
    , icon : Icon
    , enabled : Bool }


button : ButtonStyle widthUnit heightUnit -> ButtonParams msg -> Element msg
button style { msg, title, icon, enabled } =
  let
    iconStyledHtml =
      let
        iconColor =
          if enabled then
            style.iconColorEnabled
          else
            style.iconColorDisabled
        iconHtml =
          icon
          |> Icons.withSize 30
          |> Icons.toHtml []
          |> fromUnstyled
      in
      Html.Styled.div
        [ css [ Css.color (iconColor |> Utils.Color.toCss) ] ]
        [ iconHtml ]
    
    styleAttrs =
      Css.width style.width ::
      Css.height style.height ::
      Css.minWidth (Css.px defaultButtonSize) ::
      Css.minHeight (Css.px defaultButtonSize) ::

      Css.borderStyle Css.solid ::
      Css.borderRadius (Css.px buttonBorderRadius) ::
      Css.borderWidth (Css.px 1) ::

      Css.displayFlex ::
      Css.alignItems Css.center ::
      Css.justifyContent Css.center ::

      if enabled then
        [ Css.cursor Css.pointer
        , Css.borderColor (style.color |> Utils.Color.changeLight 0.7 |> Utils.Color.toCss)
        , Css.backgroundColor (style.color |> Utils.Color.toCss)
        , Css.hover [Css.backgroundColor
            (style.color |> Utils.Color.changeLight 1.15 |> Utils.Color.toCss)]
        , Css.active [Css.backgroundColor
            (style.color |> Utils.Color.changeLight 0.7 |> Utils.Color.toCss)] ]
      else
        [ Css.borderColor Css.transparent
        , Css.backgroundColor Css.transparent ]
    
    action =
      if enabled then [Utils.Events.onClickStyled msg] else []
  in
  Html.Styled.div
    ( css styleAttrs :: Attrs.title title :: action )
    [ iconStyledHtml ]
  |> toUnstyled
  |> html


defaultButtonStyle : ButtonStyle Css.Px Css.Px
defaultButtonStyle =
  { width = Css.px defaultButtonSize
  , height = Css.px defaultButtonSize
  , color = Color.rgb 0.92 0.92 0.92
  , iconColorEnabled = Color.black
  , iconColorDisabled = Color.rgb 0.5 0.5 0.5 }


defaultButton : ButtonParams msg -> Element msg
defaultButton =
  button defaultButtonStyle