module Main exposing (..)

import Model.App exposing (..)
import Update.App exposing (..)
import View.App exposing (..)

import Browser


-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = \() url key -> (init url key, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none      
