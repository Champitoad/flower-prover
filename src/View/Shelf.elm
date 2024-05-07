module View.Shelf exposing (..)

import Model.Goal exposing (..)

import View.Goal exposing (..)

import Update.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border


viewShelf : Goal -> Element Msg
viewShelf goal =
  row
    [ width fill
    , height (fillPortion 1)
    , padding 15
    , Background.gradient
        { angle = 0
        , steps = [ rgb 0.85 0.85 0.85, rgb 0.9 0.9 0.9 ] }
    , Border.color (rgb 0.6 0.6 0.6)
    , Border.widthEach { bottom = 3, left = 0, right = 0, top = 0 }
    , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 30, bottomRight = 30 }
    -- , Border.shadow
    --     { offset = (0, 5)
    --     , size = 0.1
    --     , blur = 5
    --     , color = rgb 0.3 0.3 0.3 }
    ]
    []