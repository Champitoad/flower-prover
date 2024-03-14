module View.Manual exposing (..)

import Element exposing (..)
import Element.Border as Border
import View.Style exposing (..)

backButtonStyle : List (Attribute msg)
backButtonStyle =
  [ Border.width 2
  , Border.rounded 4
  , Border.solid
  , padding 10 ]

navbar : Element msg
navbar =
  row
    [ width fill ]
    [ link
        ( alignLeft :: backButtonStyle )
        { url = "/", label = text "back to app" } ]

body : Element msg
body =
  column
    [ width (fill |> maximum 600)
    , height fill
    , centerX ]
    [ text "Look at this box:"
    ]

page : Element msg
page =
  column
    [ width fill
    , height fill
    , padding 20 ]
    [ body
    , navbar ]