module Model.App exposing (..)

import Model.Flower exposing (..)
import Model.Goal as Goal exposing (Goal, Location, Sandboxes, manualExamples)

import Url
import Browser.Navigation
import Html5.DragDrop as DnD


-- Drag-and-Drop


type alias FlowerDragId
  = { location : Location, source : Zipper, content : Flower }

type alias FlowerDropId
  = Maybe { location : Location, target : Zipper, content : Bouquet }

type alias FlowerDnD
  = DnD.Model FlowerDragId FlowerDropId

type alias FlowerDnDMsg
  = DnD.Msg FlowerDragId FlowerDropId


-- Full state of the application


type alias Model
  = { goal : Goal
    , history : History
    , manualExamples : Sandboxes
    , dragDrop : FlowerDnD
    , url : Url.Url
    , key : Browser.Navigation.Key }


init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
  { goal = Goal.fromBouquet [orElim]
  , history = History { prev = Nothing, next = Nothing }
  , manualExamples = manualExamples
  , dragDrop = DnD.init
  , url = url
  , key = key
  }


getGoal : Location -> Model -> Goal
getGoal location model =
  case location of
    Goal.App ->
      model.goal
    Goal.Manual sandboxID ->
      (Goal.getSandbox sandboxID model.manualExamples).currentGoal


setGoal : Location -> Goal -> Model -> Model
setGoal location goal model =
  case location of
    Goal.App ->
      { model | goal = goal }
    Goal.Manual sandboxID ->
      { model | manualExamples = Goal.updateSandbox sandboxID goal model.manualExamples }


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