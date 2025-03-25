port module Update.App exposing (..)

import Update.Rules exposing (..)

import Model.Flower exposing (..)
import Model.Goal exposing (..)
import Model.App exposing (..)

import Json.Decode exposing (Value)      

import Html5.DragDrop as DnD

import Keyboard.Event exposing (KeyboardEvent)

import Url
import Browser
import Browser.Navigation
import View.Route as Route


port dragstart : Value -> Cmd msg


type Msg
  = Action Rule Location Zipper Bouquet
  | Auto
  | SetGoal Bouquet
  | ChangeUIMode UIMode
  | Undo
  | Redo
  | DragDropMsg FlowerDnDMsg
  | ResetSandbox SandboxID
  | HandleKeyboardEvent KeyboardEvent
  | ConsoleLog String String
  | DoNothing
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest


handleDragDropMsg : FlowerDnDMsg -> Model -> (Model, Cmd Msg)
handleDragDropMsg dndMsg model =
  let
    dragStart = 
      DnD.getDragstartEvent dndMsg

    cmd =
      dragStart
      |> Maybe.map (.event >> dragstart)
      |> Maybe.withDefault Cmd.none

    ( newDragDrop, result ) =
      DnD.update dndMsg model.dragDrop

    model_ =
      case dragStart of
        Just { dragId } ->
          let
            goal = getGoal dragId.location model
            
            newMode =
              case goal.mode of
                ProofMode Justifying ->
                  ProofMode Importing
                EditMode _ surgery ->
                  EditMode Reordering surgery
                _ ->
                  goal.mode
            
            newGoal =
              { goal | mode = newMode }
            
            newModel =
              { model | dragDrop = newDragDrop }
          in
          setGoal dragId.location newGoal newModel

        Nothing ->
          case result of
            Just (drag, drop, _) ->
              let
                goal = getGoal drag.location model
                
                defaultMode =
                  case goal.mode of
                    ProofMode _ ->
                      ProofMode Justifying

                    EditMode _ surgery ->
                      EditMode Operating surgery

                    _ ->
                      goal.mode
              in
              case drop of
                -- Dropping on target
                Just destination ->
                  let
                    action =
                      case goal.mode of
                        ProofMode Importing ->
                          Action Import goal.location
                            destination.target [drag.content]
                        
                        EditMode Reordering _ ->
                          Action Reorder goal.location
                            destination.target destination.content
                        
                        _ ->
                          DoNothing
                    
                    updatedModel =
                      update action model |> Tuple.first
                    
                    updatedGoal =
                      getGoal goal.location updatedModel
                    
                    newGoal =
                      { updatedGoal | mode = defaultMode }
                    
                    newModel =
                      setGoal goal.location newGoal updatedModel
                  in
                  { newModel | dragDrop = newDragDrop }

                -- Dropping on non-target
                Nothing ->
                  let newModel = setGoal goal.location { goal | mode = defaultMode } model in
                  { newModel | dragDrop = newDragDrop }
        
            -- Dragging
            Nothing ->
              { model | dragDrop = newDragDrop }
  in
  (model_, cmd)


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ goal, manualExamples } as model) =
  case msg of
    Action rule location zipper bouquet ->
      let
        newFocus =
          apply rule zipper bouquet

        newMode =
          case goal.mode of
            EditMode interaction surgery ->
              EditMode interaction (operate rule zipper bouquet surgery)
            _ ->
              goal.mode
        
        oldGoal =
          case location of
            App -> goal
            Manual sandboxID -> (getSandbox sandboxID manualExamples).currentGoal
              
        newGoal =
            { oldGoal | focus = newFocus, mode = newMode }
        
        newModel =
          case location of
            App ->
              { model
              | goal = newGoal
              , history = History { prev = Just model, next = Nothing }
              }
            
            Manual sandboxID ->
              { model
              | manualExamples = updateSandbox sandboxID newGoal manualExamples
              }
      in
      (newModel, Cmd.none)
    
    Auto ->
      ( { model | goal =
          { goal
          | focus = auto [Unlock, Decompose, Close, Justify] goal.focus
          }
        , history = History { prev = Just model, next = Nothing } }
      , Cmd.none )
    
    SetGoal bouquet ->
      ( { model | goal = { goal | focus = bouquet } }, Cmd.none )
    
    ChangeUIMode mode ->
      let
        newFocus =
          case mode of
            ProofMode _ ->
              List.map naturalizeFlower goal.focus
            _ ->
              goal.focus
      in
      ({ model | goal =
         { goal
         | mode = mode
         , focus = newFocus
         }
       }, Cmd.none)
    
    Undo ->
      (undo model, Cmd.none)

    Redo ->
      (redo model, Cmd.none)

    DragDropMsg dndMsg ->
      handleDragDropMsg dndMsg model
    
    ResetSandbox id ->
      ( { model | manualExamples = resetSandbox id model.manualExamples }
      , Cmd.none )
    
    HandleKeyboardEvent { ctrlKey, key } ->
      let
        newModel =
          case (ctrlKey, key) of
            (True, Just "z") -> update Undo model |> Tuple.first
            (True, Just "y") -> update Redo model |> Tuple.first
            _ -> model
      in
      (newModel, Cmd.none)
    
    ConsoleLog tag message ->
      let _ = Debug.log tag message in
      (model, Cmd.none)
    
    DoNothing ->
      (model, Cmd.none)
    
    UrlChanged url ->
      let
        newModel =
          case Route.fromUrl url of
            Route.Manual ->
              { model | manualExamples = resetAllSandboxes model.manualExamples }
            _ ->
              model
      in
      ({ newModel | url = url }, Cmd.none)
    
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          let 
            newPath =
              case url.host of
                "www.lix.polytechnique.fr" ->
                  "/Labo/Pablo.DONATO/flowerprover" ++ url.path
                "pablogician.refl.fr" ->
                  let _ = Debug.log "[Routing]" "You are on the epidictic server!" in
                  "/flowerprover" ++ url.path
                _ ->
                  let _ = Debug.log "[Routing]" "I don't know this place," ++ url.path ++ "?" in
                  url.path
            
            newUrl =
              { url | path = newPath }  
          in
          ( model, Browser.Navigation.pushUrl model.key (Url.toString newUrl) )

        Browser.External href ->
          ( model, Browser.Navigation.load href )
