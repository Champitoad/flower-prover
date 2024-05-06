module View.Shelf exposing (..)

import Model.Goal exposing (..)

import View.Goal exposing (..)

import Update.App exposing (..)

import Element exposing (..)


viewShelf : Goal -> Element Msg
viewShelf goal =
  el [] none