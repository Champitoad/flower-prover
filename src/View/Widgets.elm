module View.Widgets exposing (..)

import Utils.Color
import Utils.Events

import Element exposing (..)
import Element.Background
import Element.Font

import Css
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Attributes as Attrs exposing (css)

import FeatherIcons as Icons
import FeatherIcons exposing (Icon)

import Color
import Html.Styled.Attributes exposing (action)
import Css exposing (enabled)


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

type ButtonAction msg
  = Msg msg
  | Link String

type alias ButtonParams msg
  = { action : ButtonAction msg
    , title : String
    , icon : Icon
    , enabled : Bool }


button : ButtonStyle widthUnit heightUnit -> ButtonParams msg -> Element msg
button style { action, title, icon, enabled } =
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
    
    tag =
      case action of
        Msg _ ->
          Html.Styled.div
        Link _ ->
          Html.Styled.a

    actionAttr =
      if enabled then
        case action of
          Msg msg ->
            [ Utils.Events.onClickStyled msg ]
          Link url ->
            [ Attrs.href url ]
      else
        []
  in
  tag
    ( css styleAttrs :: Attrs.title title :: actionAttr )
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


fullPageTextMessage : String -> Element msg
fullPageTextMessage txt =
  el
    [ width fill
    , height fill
    , Element.Background.color (rgb 1 1 1) ]
    ( el
        [ centerX, centerY
        , Element.Font.size 50 ]
        ( text txt ) )