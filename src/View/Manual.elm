module View.Manual exposing (..)

import Element exposing (..)

navbar : Element msg
navbar =
  row
    [ width fill ]
    [ link
        [ alignLeft ]
        { url = "/", label = text "back to app" } ]

body : Element msg
body =
  column
    [ width (fill |> maximum 800) ]
    [ ]

page : Element msg
page =
  column
    [ width fill ]
    [ navbar
    , body ]