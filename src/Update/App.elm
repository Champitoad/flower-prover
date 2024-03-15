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
handleDragDropMsg dndMsg ({ goal } as model) =
  let
    dragStart = 
      DnD.getDragstartEvent dndMsg

    cmd =
      dragStart
      |> Maybe.map (.event >> dragstart)
      |> Maybe.withDefault Cmd.none

    ( newDragDrop, result ) =
      DnD.update dndMsg goal.dragDrop

    model_ =
      case dragStart of
        Just _ ->
          case goal.mode of
            ProofMode Justifying ->
              { model | goal =
                { goal
                | dragDrop = newDragDrop
                , mode = ProofMode Importing
                }
              }

            EditMode _ surgery ->
              { model | goal =
                { goal
                | dragDrop = newDragDrop
                , mode = EditMode Reordering surgery
                }
              }
            
            _ ->
              model

        Nothing ->
          let
            defaultMode =
              case goal.mode of
                ProofMode _ ->
                  ProofMode Justifying

                EditMode _ surgery ->
                  EditMode Operating surgery

                _ ->
                  goal.mode
          in
          case result of
            Just (drag, drop, _) ->
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
                    
                    newModel =
                      update action model |> Tuple.first
                    
                    newGoal =
                      newModel.goal
                  in
                  { newModel | goal = { newGoal | dragDrop = newDragDrop, mode = defaultMode } }

                -- Dropping on non-target
                Nothing ->
                  { model | goal = { goal | dragDrop = newDragDrop, mode = defaultMode } }
        
            -- Dragging
            Nothing ->
              { model | goal = { goal | dragDrop = newDragDrop } }
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
      ({ model | url = url }, Cmd.none)
    
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Browser.Navigation.load href )
