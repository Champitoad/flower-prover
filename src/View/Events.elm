module View.Events exposing (..)

import Model.Flower exposing (..)

import Update.App exposing (..)

import Utils.Events exposing (..)

import Element exposing (..)

import Html.Attributes exposing (..)

import Html5.DragDrop as DnD


stopPropagation : List (Attribute Msg)
stopPropagation =
  [ onDragOver DoNothing
  , onMouseMove DoNothing ]


dragAction : Zipper -> Flower -> List (Attribute Msg)
dragAction zipper flower =
  if List.length zipper <= 1 then []
  else
    List.map htmlAttribute <|
    DnD.draggable DragDropMsg
      { source = zipper, content = flower }