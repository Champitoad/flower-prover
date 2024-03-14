module Model.App exposing (..)

import Model.Flower exposing (..)
import Model.Goal as Goal

import Url
import Browser.Navigation



-- Full state of the application


type alias Model
  = { goal : Goal.Goal
    , history : History
    , url : Url.Url
    , key : Browser.Navigation.Key }


init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
  { goal = Goal.fromBouquet [orElim]
  , history = History { prev = Nothing, next = Nothing }
  , url = url
  , key = key
  }


-- History of the full state mutually defined


type History
  = History { prev : Maybe Model
            , next : Maybe Model }


getHistory : Model ->  { prev : Maybe Model, next : Maybe Model }
getHistory model =
  let (History history) = model.history in
  history


setHistory : { prev : Maybe Model, next : Maybe Model } -> Model -> Model
setHistory history model =
  { model | history = History history }


undo : Model -> Model
undo model =
  case (getHistory model).prev of
    Just prevModel ->
      let prevHistory = getHistory prevModel in
      setHistory { prevHistory | next = Just model } prevModel
    Nothing ->
      model


redo : Model -> Model
redo model =
  case (getHistory model).next of
    Just nextModel ->
      nextModel
    Nothing ->
      model