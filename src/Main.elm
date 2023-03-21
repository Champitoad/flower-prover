port module Main exposing (..)

import Utils.List
import Utils.Events exposing (..)
import Utils.Color as Color

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events

import Html exposing (Html)
import Html.Attributes exposing (style, title)

import Html5.DragDrop as DnD

import Json.Decode exposing (Value)      

import FeatherIcons as Icons


-- MAIN

port dragstart : Value -> Cmd msg


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none      


-- MODEL


type Flower
  = Atom String
  | Flower Garden (List Garden)

type alias Bouquet
  = List Flower

type Garden
  = Garden Bouquet


type Zip
  = Bouquet Bouquet Bouquet
  | Pistil (List Garden)
  | Petal Garden (List Garden) (List Garden)

type alias Zipper
  = List Zip

-- We encode zippers as lists, where the head is the innermost context


fillZip : Zip -> Bouquet -> Bouquet
fillZip zip bouquet =
  case zip of
    Bouquet left right ->
      left ++ bouquet ++ right

    Pistil petals ->
      [Flower (Garden bouquet) petals]

    Petal pistil leftPetals rightPetals ->
      [Flower pistil (leftPetals ++ Garden bouquet :: rightPetals)]


fillZipper : Bouquet -> Zipper -> Bouquet
fillZipper =
  List.foldl fillZip


hypsZip : Zip -> Bouquet
hypsZip zip =
  case zip of
    Bouquet left right ->
      left ++ right

    Pistil _ ->
      []
    
    Petal (Garden bouquet) _ _ ->
      bouquet


hypsZipper : Zipper -> Bouquet
hypsZipper zipper =
  List.foldl (\zip acc -> hypsZip zip ++ acc) [] zipper


isHypothesis : Flower -> Zipper -> Bool
isHypothesis flower zipper =
  List.member flower (hypsZipper zipper)


justifies : Zipper -> Zipper -> Bool
justifies source destination =
  let lca = Utils.List.longestCommonSuffix source destination in
  case source of
    -- Self pollination
    Bouquet _ _ :: (Pistil _ :: grandParent as parent) ->
      lca == grandParent ||
      lca == parent

    -- Wind pollination
    Bouquet _ _ :: parent ->
      lca == parent
    
    _ ->
      False


type Polarity
  = Pos
  | Neg


invert : Polarity -> Polarity
invert polarity =
  case polarity of
    Pos -> Neg
    Neg -> Pos


type alias Context
  = { zipper : Zipper,
      polarity : Polarity }


type alias Selection
  = List Zipper
 

type alias FlowerDragId
  = { source : Zipper, content : Flower }

type alias FlowerDropId
  = Maybe { target : Zipper, content : Bouquet }

type alias FlowerDnD
  = DnD.Model FlowerDragId FlowerDropId

type alias FlowerDnDMsg
  = DnD.Msg FlowerDragId FlowerDropId


type ProofInteraction
  = Justifying
  | Importing
  | Fencing Selection


type EditInteraction
  = Erasing
  | Adding Zipper
  | Reordering


type UIMode
  = ProofMode ProofInteraction
  | EditMode EditInteraction
  | NavigationMode


type alias Model
  = { goal : Bouquet
    , mode : UIMode
    , dragDrop : FlowerDnD }


yinyang : Flower
yinyang =
  Flower (Garden []) [Garden []]


identity : Flower
identity =
  Flower
    (Garden [Atom "a"])
    [Garden [Atom "a"]]


yvonne : Flower
yvonne =
  Flower
    ( Garden
        [ Flower
            (Garden [Atom "b"])
            [Garden [Atom "c"]] ] )
    [ Garden
        [ Atom "a"
        , Flower
            (Garden [Atom "b"])
            [Garden [Atom "c"]] ]]


bigFlower : Flower
bigFlower =
  Flower
    ( Garden
        [ Atom "a"
        , Flower
            ( Garden
                [ Atom "a" ] )
            [ Garden
                [ Atom "b" ],
              Garden
                [ Flower
                    ( Garden
                        [ Atom "b" ] )
                    [ Garden
                        [ Atom "c" ] ],
                  Atom "b" ] ]
        , Flower
            ( Garden
                [ Atom "d"] )
            [ Garden
                [ Atom "e" ] ] ] )
    [ Garden
        [ Atom "b"
        , Atom "a" ]
    , Garden
      [ Atom "c" ] ]


implies : String -> String -> Flower
implies a b =
  Flower
    ( Garden
        [ Atom a ] )
    [ Garden
        [ Atom b ] ]

modusPonensCurryfied : Flower
modusPonensCurryfied =
  Flower
    ( Garden [ implies "a" "b" ] ) 
    [ Garden [ implies "a" "b" ] ]


notFalse : Flower
notFalse =
  Flower (Garden [Flower (Garden []) []]) []


criticalPair : Flower
criticalPair =
  Flower
    ( Garden
        [ Flower
            ( Garden [] )
            [ Garden [ Atom "a" ]
            , Garden [ Atom "b" ] ]
        , implies "a" "c"
        , implies "b" "c" ] )
    [ Garden [ Atom "c" ] ]


init : () -> (Model, Cmd Msg)
init () =
  ( { goal = [ criticalPair ]
    , mode = EditMode Erasing
    , dragDrop = DnD.init }
  , Cmd.none )


-- UPDATE

type Rule
  = Justify -- down pollination
  | Import -- up pollination
  | Unlock -- empty pistil
  | Close -- empty petal
  | Fence -- fencing
  | Reorder -- multiset


type Msg
  = Action Rule Bouquet Zipper
  | DragDropMsg FlowerDnDMsg
  | ChangeUIMode UIMode
  | DoNothing


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Action rule bouquet zipper ->
      let
        newGoal =
          case (rule, bouquet, zipper) of
            (Justify, _, _) ->
              fillZipper [] zipper
            
            (Import, _, _) ->
              fillZipper bouquet zipper

            (Unlock, [], Pistil [Garden petal] :: parent)  ->
              fillZipper petal parent
            
            (Unlock, [], Pistil branches :: Bouquet left right :: Pistil petals :: parent) ->
              let
                case_ : Garden -> Flower
                case_ branch =
                  Flower branch petals
                
                pistil =
                  Garden (left ++ right)
                
                cases =
                  List.map case_ branches  
              in
              fillZipper [Flower pistil [Garden cases]] parent
            
            (Close, [], Petal _ _ _ :: parent) ->
              fillZipper [] parent
            
            (Reorder, _, _ :: parent) ->
              fillZipper bouquet parent

            _ ->
              model.goal
      in
      ({ model | goal = newGoal }, Cmd.none)

    DragDropMsg dndMsg ->
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
                  { model | dragDrop = newDragDrop, mode = ProofMode Importing }

                EditMode Erasing ->
                  { model | dragDrop = newDragDrop, mode = EditMode Reordering }
                
                _ ->
                  model

            Nothing ->
              let
                defaultMode =
                  case model.mode of
                    ProofMode _ -> ProofMode Justifying
                    EditMode _ -> EditMode Erasing
                    _ -> model.mode
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
                                [drag.content] destination.target
                            
                            EditMode Reordering ->
                              Action Reorder
                                destination.content destination.target
                            
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
    
    ChangeUIMode mode ->
      ({ model | mode = mode }, Cmd.none)
    
    DoNothing ->
      (model, Cmd.none)

-- VIEW


---- Text


viewFlowerText : Flower -> String
viewFlowerText flower =
  case flower of
    Atom name ->
      name    
    
    Flower pistil petals ->
      let
        pistilText =
          viewGardenText pistil

        petalsText =
          petals
          |> List.map viewGardenText
          |> String.join "; "
      in
      "(" ++ pistilText ++ " ⫐ " ++ petalsText ++ ")"


viewGardenText : Garden -> String
viewGardenText (Garden bouquet) =
  bouquet
  |> List.map viewFlowerText
  |> String.join ", "


viewZipperText : Zipper -> String
viewZipperText zipper =
  fillZipper [Atom "□"] zipper
  |> List.map (viewFlowerText)
  |> String.join ", "

logZipper : String -> Zipper -> String
logZipper msg zipper =
  zipper
  |> viewZipperText
  |> Debug.log msg

logBouquet : String -> Bouquet -> String
logBouquet msg bouquet =
  bouquet
  |> List.map viewFlowerText
  |> String.join ", "
  |> Debug.log msg


---- Graphics


transparent : Color
transparent =
  rgba 0 0 0 0


flowerForegroundColor : Polarity -> Color
flowerForegroundColor polarity =
  case polarity of
    Pos ->
      rgb 0 0 0
    Neg ->
      rgb 1 1 1

flowerBackgroundColor : Polarity -> Color
flowerBackgroundColor polarity =
  case polarity of
    Pos ->
      rgb 1 1 1
    Neg ->
      rgb 0 0 0


flowerBorderWidth : Int
flowerBorderWidth =
  3

flowerBorderRound : Int
flowerBorderRound =
  10


type alias ZoneStyle msg
  = { borderWidth : Int
    , active : List (Attribute msg)
    , inactive : List (Attribute msg) }


actionable : ZoneStyle msg
actionable =
  let
    width =
      3

    border =
      [ Border.width 5
      , Border.dotted
      , Border.rounded flowerBorderRound ]
  in
  { borderWidth = width
  , active =
      pointer ::
      Border.color (rgb 0.3 0.9 0.3) ::
      Background.color (rgba 0.3 0.9 0.3 0.5) ::
      border
  , inactive =
      Border.color transparent ::
      border }


droppable : Color.Color -> ZoneStyle msg
droppable color =
  let
    width =
      3

    border =
      [ Border.width width
      , Border.dashed
      , Border.rounded flowerBorderRound
      , Border.color (Color.toElement color) ]
    
    bgColor =
      Color.withAlpha 0.5 color |> Color.toElement
  in
  { borderWidth = width
  , active = Background.color bgColor :: border
  , inactive = border }


stopPropagation : List (Attribute Msg)
stopPropagation =
  [ onDragOver DoNothing
  , onMouseMove DoNothing ]


nonSelectable : Attribute Msg
nonSelectable =
  htmlAttribute <| style "user-select" "none"


viewAtom : Model -> Context -> String -> Element Msg
viewAtom model context name =
  let
    atom =
      Atom name

    clickAction =
      case model.mode of
        ProofMode Justifying ->
          if isHypothesis atom context.zipper then
            (Events.onClick (Action Justify [atom] context.zipper))
            :: actionable.active
          else
            actionable.inactive

        _ ->
          actionable.inactive
  in
  el
    ( [ centerX, centerY
      , padding 10
      , Font.color (flowerForegroundColor context.polarity)
      , Font.size 50
      , nonSelectable ]
      ++ clickAction )
    ( text name )


viewPistil : Model -> Context -> Garden -> List Garden -> Element Msg
viewPistil model context (Garden bouquet as pistil) petals =
  let
    newZipper =
      Pistil petals :: context.zipper

    clickAction =
      case model.mode of
        ProofMode Justifying ->
          let
            action =
              (Events.onClick (Action Unlock bouquet newZipper))
              :: actionable.active
          in
          if List.isEmpty bouquet then
            case context.zipper of
              _ :: Pistil _ :: _ ->
                action
              _ ->
                if List.length petals == 1 then
                  action
                else
                  actionable.inactive
          else
            actionable.inactive
        
        _ ->
          actionable.inactive
  in
  el
    ( [ width fill
      , height fill
      , Border.rounded flowerBorderRound ])
    ( el
        ( [ width fill
          , height fill
          , padding 10 ]
         ++ clickAction )
        ( viewGarden
          model
          { context
          | zipper = newZipper
          , polarity = invert context.polarity }
          pistil ) )


viewPetal : Model -> Context -> Garden -> (List Garden, List Garden) -> Garden -> Element Msg
viewPetal model context pistil (leftPetals, rightPetals) (Garden bouquet as petal) =
  let
    newZipper =
      Petal pistil leftPetals rightPetals :: context.zipper

    clickAction =
      case model.mode of
        ProofMode Justifying ->
          if List.isEmpty bouquet then
            (Events.onClick (Action Close bouquet newZipper))
            :: actionable.active
          else
            actionable.inactive
        
        _ ->
          actionable.inactive
  in
  el
    [ width fill
    , height fill
    , Border.rounded flowerBorderRound
    , Background.color (flowerBackgroundColor context.polarity) ]
    ( el
        ( [ width fill
          , height fill
          , padding 10 ]
         ++ clickAction )
        ( viewGarden
            model
            { context
            | zipper = newZipper }
            petal ) )


viewFlower : Model -> Context -> Flower -> Element Msg
viewFlower model context flower =
  case flower of
    Atom name ->
      viewAtom model context name
    
    Flower pistil petals ->
      let
        pistilEl =
          viewPistil model context pistil petals
        
        petalsEl =
          row
            [ width fill
            , height fill
            , spacing flowerBorderWidth ]
            ( Utils.List.zipperMap (viewPetal model context pistil) petals )

        dragAction =
          if List.length context.zipper <= 1 then []
          else
            List.map htmlAttribute <|
            DnD.draggable DragDropMsg
              { source = context.zipper, content = flower }
      in
      column
        ( [ width fill
          , height fill
          , padding flowerBorderWidth
          , Background.color (flowerForegroundColor context.polarity)
          , Border.color (flowerForegroundColor context.polarity)
          , Border.rounded flowerBorderRound
          , Border.shadow
              { offset = (0, 5)
              , size = 0.25
              , blur = 15
              , color = flowerForegroundColor context.polarity } ]
        ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
        ++ dragAction )
        [ pistilEl, petalsEl ]


viewGarden : Model -> Context -> Garden -> Element Msg
viewGarden model context (Garden bouquet) =
  let
    flowerEl (left, right) =
      viewFlower
        model
        { context
        | zipper = Bouquet left right :: context.zipper }
    
    dropAction (left, right) =
      case model.mode of
        ProofMode Importing ->
          case DnD.getDragId model.dragDrop of
            Just { source } ->
              -- if isHypothesis content context.zipper then
              if justifies source context.zipper then
                let
                  dropStyle =
                    droppable (Color.fromRgb { red = 1, green = 0.8, blue = 0 })

                  dropTargetStyle =
                    case DnD.getDropId model.dragDrop of
                      Just (Just { target }) ->
                        if Bouquet left right :: context.zipper == target
                        then dropStyle.active
                        else dropStyle.inactive
                    
                      _ ->
                        dropStyle.inactive
                in
                dropTargetStyle ++
                ( List.map htmlAttribute <|
                  DnD.droppable DragDropMsg
                    (Just { target = Bouquet left right :: context.zipper
                          , content = [] }) )
              else
                []
            _ ->
              []

        EditMode Reordering ->
          case DnD.getDragId model.dragDrop of
            Just { source, content } ->
              case source of
                Bouquet sourceLeft sourceRight :: sourceParent ->
                  if sourceParent == context.zipper then
                    let
                      (sourceIndex, index) =
                        (List.length sourceLeft, List.length left)
                    in
                    if sourceIndex == index || index == sourceIndex + 1 then []
                    else
                      let
                        whole =
                          left ++ right

                        newBouquet =
                          if sourceIndex < index then
                            let
                              middle =
                                Utils.List.slice (sourceIndex + 1) (index - 1) whole
                            in
                            sourceLeft ++ middle ++ content :: right
                          else
                            let
                              middle =
                                Utils.List.slice index (sourceIndex - 1) whole
                            in
                            left ++ content :: (middle ++ sourceRight)

                        dropStyle =
                          droppable (Color.fromRgb { red = 0.7, green = 0.7, blue = 0.7 })

                        dropTargetStyle =
                          case DnD.getDropId model.dragDrop of
                            Just (Just { target }) ->
                              let _ = Debug.log "source index" sourceIndex in
                              let _ = Debug.log "target index" index in
                              if Bouquet left right :: context.zipper == target
                              then dropStyle.active
                              else dropStyle.inactive

                            _ ->
                              dropStyle.inactive
                      in
                      dropTargetStyle ++
                      ( List.map htmlAttribute <|
                        DnD.droppable DragDropMsg
                          (Just { target = Bouquet left right :: context.zipper
                                , content = newBouquet }) )
                  else
                    []
                _ ->
                  []
            _ ->
              []
        _ ->
          []

    spaceSize = 30
    
    layoutAttrs =
      [ (width (fill |> minimum spaceSize))
      , (height (fill |> minimum spaceSize))
      , padding spaceSize
      , spacing spaceSize ]
    
    borderAttrs =
      [ Border.width (droppable Color.transparent).borderWidth
      , Border.color transparent ]

    length flower =
      case flower of
        Atom _ -> shrink
        Flower _ _ -> fill

    intersticial () =
      let
        attrs =
          layoutAttrs ++
          borderAttrs

        dropZone lr =
          el
            ( [ width (spaceSize |> px)
              , height (fill |> minimum spaceSize) ]
            ++ borderAttrs
            ++ dropAction lr )
            none

        sperse ((left, right) as lr) flower =
          let
            lastDropzone =
              if List.isEmpty right
              then [onRight (dropZone (left ++ [flower], right))]
              else []
          in
          el
            ( [ width (length flower)
              , height fill
              , centerX, centerY
              , onLeft (dropZone (left, flower :: right)) ]
              ++ lastDropzone )
            ( flowerEl lr flower )

        els =
          Utils.List.zipperMap sperse bouquet
      in
      wrappedRow attrs els
        
    normal () =
      let
        attrs =
          layoutAttrs ++
          borderAttrs ++
          dropAction (bouquet, [])
        
        sperse lr flower =
          el
            [ width (length flower)
            , height fill
            , centerX, centerY ]
            ( flowerEl lr flower )

        els =
          Utils.List.zipperMap sperse bouquet
      in
      wrappedRow attrs els
  in
  case model.mode of
    EditMode _ ->
      intersticial ()

    _ ->
      normal ()


viewGoal : Model -> Element Msg
viewGoal model =
  -- text (viewFlowerText model)
  let
    bouquetEls () =
      ( Utils.List.zipperMap
          ( \(l, r) flower ->
              el [ width fill, height fill, centerX, centerY ]
              ( viewFlower model (Context [Bouquet l r] Pos) flower ) )
          model.goal )
    
    bouquetEl () =
      column
        [ width fill
        , height fill
        , scrollbarY
        , spacing 100
        , Background.color (rgb 0.65 0.65 0.65) ]
        ( bouquetEls () )

    workingOnIt =
      el
        [ width fill
        , height fill
        , Background.color (rgb 1 1 1) ]
        ( el
            [ centerX, centerY
            , Font.size 50 ]
            ( text "Working on it!" ) )
  in
  case model.mode of
    ProofMode _ ->
      bouquetEl ()
    
    EditMode _ ->
      bouquetEl ()

    _ ->
      workingOnIt


type ModeSelectorPosition
  = Start | Middle | End


viewModeSelector : UIMode -> Element Msg
viewModeSelector currentMode =
  let
    borderRoundSize = 10

    item mode position =
      let
        isSelected =
          case (mode, currentMode) of          
            (ProofMode _, ProofMode _) -> True
            (EditMode _, EditMode _) -> True
            _ -> mode == currentMode

        (bgColor, fgColor) =
          if isSelected
          then (rgb255 58 134 255, rgb 1 1 1)
          else (rgb 1 1 1, rgb 0 0 0)

        (iconEl, titleText) =
          let
            (title, icon) =
              case mode of
                ProofMode _ -> ("Prove", Icons.checkSquare)
                EditMode _ -> ("Edit", Icons.edit2)
                NavigationMode -> ("Navigate", Icons.navigation)
            elem =
              el
                [ centerX, centerY ]
                ( icon
                  |> Icons.withSize 30
                  |> Icons.toHtml [ fgColor |> Color.fromElement |> Color.toHtml ]
                  |> html )
          in
          (elem, title)
        
        borderRound =
          case position of
            Start ->
              { topLeft = borderRoundSize, bottomLeft = borderRoundSize
              , topRight = 0, bottomRight = 0 }
            Middle ->
              { topLeft = 0, bottomLeft = 0
              , topRight = 0, bottomRight = 0 }
            End ->
              { topLeft = 0, bottomLeft = 0
              , topRight = borderRoundSize, bottomRight = borderRoundSize }

        changeAction =
          [ Events.onClick (ChangeUIMode mode)
          , pointer ]
      in
      el
        ( [ width (60 |> px)
          , height (55 |> px)
          , Background.color bgColor
          , Border.roundEach borderRound
          , htmlAttribute <| title titleText ]
         ++ changeAction )
        iconEl
    
    borderColor = rgb 0.6 0.6 0.6
  in
  row
    [ width shrink
    , height fill
    , centerX, centerY
    , spacing 1
    , Border.width 0
    , Border.rounded borderRoundSize
    , Border.color borderColor
    , Background.color borderColor ]
    [ item (ProofMode Justifying) Start
    , item (EditMode Erasing) Middle
    , item NavigationMode End ]


viewToolbar : Model -> Element Msg
viewToolbar model =
  row
    [ width fill
    , height shrink
    , padding 15
    , Background.color (rgb 0.9 0.9 0.9) ]
    [ viewModeSelector model.mode ]


view : Model -> Html Msg
view model =
  let
    goal =
      viewGoal model
    
    toolbar =
      viewToolbar model
    
    app =
      column
        [ width fill,
          height fill ]
        [goal, toolbar]
  in
  layout [] app