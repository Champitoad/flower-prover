module Model.App exposing (..)

import Model.Flower exposing (..)

import Html5.DragDrop as DnD
import Url
import Browser.Navigation


-- Selection


type alias Selection
  = List Zipper


-- Drag-and-Drop


type alias FlowerDragId
  = { source : Zipper, content : Flower }

type alias FlowerDropId
  = Maybe { target : Zipper, content : Bouquet }

type alias FlowerDnD
  = DnD.Model FlowerDragId FlowerDropId

type alias FlowerDnDMsg
  = DnD.Msg FlowerDragId FlowerDropId


-- Modal UI


type ProofInteraction
  = Justifying
  | Importing
  | Fencing Selection


type alias Surgery =
  { cropped : Maybe Flower
  , pulled : Maybe Garden }


initialSurgery : Surgery
initialSurgery =
  { cropped = Nothing
  , pulled = Nothing }


type EditInteraction
  = Operating
  | Adding Zipper
  | Reordering


type UIMode
  = ProofMode ProofInteraction
  | EditMode EditInteraction Surgery
  | NavigationMode


-- Full state of the application


type alias Model
  = { goal : Bouquet
    , mode : UIMode
    , dragDrop : FlowerDnD
    , history : History
    , url : Url.Url
    , key : Browser.Navigation.Key }


init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
  { goal = [orElim]
  , mode = ProofMode Justifying
  , dragDrop = DnD.init
  , history = History { prev = Nothing, next = Nothing }
  , url = url
  , key = key }


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