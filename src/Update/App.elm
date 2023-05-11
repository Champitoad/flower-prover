port module Update.App exposing (..)

import Update.Rules exposing (..)

import Model.Flower exposing (..)
import Model.Goal as Goal
import Model.App exposing (..)

import Json.Decode exposing (Value)      

import Html5.DragDrop as DnD

import Keyboard.Event exposing (KeyboardEvent)


port dragstart : Value -> Cmd msg


type Msg
  = Action Rule Zipper Bouquet
  | Auto
  | ChangeUIMode UIMode
  | Undo
  | Redo
  | DragDropMsg FlowerDnDMsg
  | HandleKeyboardEvent KeyboardEvent
  | DoNothing


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
        Just _ ->
          case model.mode of
            ProofMode Justifying ->
              { model
              | dragDrop = newDragDrop
              , mode = ProofMode Importing }

            EditMode _ surgery ->
              { model
              | dragDrop = newDragDrop
              , mode = EditMode Reordering surgery }
            
            _ ->
              model

        Nothing ->
          let
            defaultMode =
              case model.mode of
                ProofMode _ ->
                  ProofMode Justifying

                EditMode _ surgery ->
                  EditMode Operating surgery

                _ ->
                  model.mode
          in
          case result of
            Just (drag, drop, _) ->
              case drop of
                -- Dropping on target
                Just destination ->
                  let
                    action =
                      case model.mode of
                        ProofMode Importing ->
                          Action Import
                            destination.target [drag.content]
                        
                        EditMode Reordering _ ->
                          Action Reorder
                            destination.target destination.content
                        
                        _ ->
                          DoNothing
                    
                    newModel =
                      update action model |> Tuple.first
                  in
                  { newModel | dragDrop = newDragDrop, mode = defaultMode }

                -- Dropping on non-target
                Nothing ->
                  { model | dragDrop = newDragDrop, mode = defaultMode }
        
            -- Dragging
            Nothing ->
              { model | dragDrop = newDragDrop }
  in
  (model_, cmd)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Action rule zipper bouquet ->
      let
        mode =
          case model.mode of
            EditMode interaction surgery ->
              EditMode interaction (operate rule zipper bouquet surgery)
            _ ->
              model.mode
      in
      ( { model
        | goal = Goal.fromBouquet <| apply rule zipper bouquet
        , mode = mode
        , history = History { prev = Just model, next = Nothing } }
      , Cmd.none )
    
    Auto ->
      ( { model
        | goal = Goal.map (auto [Unlock, Decompose, Close, Justify]) model.goal
        , history = History { prev = Just model, next = Nothing } }
      , Cmd.none )
    
    ChangeUIMode mode ->
      let
        newGoal =
          case mode of
            ProofMode _ ->
              Goal.map (List.map naturalizeFlower) model.goal
            _ ->
              model.goal
      in
      ({ model | mode = mode, goal = newGoal }, Cmd.none)
    
    Undo ->
      (undo model, Cmd.none)

    Redo ->
      (redo model, Cmd.none)

    DragDropMsg dndMsg ->
      handleDragDropMsg dndMsg model
    
    HandleKeyboardEvent { ctrlKey, key } ->
      let
        newModel =
          case (ctrlKey, key) of
            (True, Just "z") -> update Undo model |> Tuple.first
            (True, Just "y") -> update Redo model |> Tuple.first
            _ -> model
      in
      (newModel, Cmd.none)
    
    DoNothing ->
      (model, Cmd.none)