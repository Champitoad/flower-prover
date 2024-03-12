module View.Goal exposing (..)

import View.Style as Style exposing (..)
import View.Widgets exposing (..)
import View.Events exposing (..)

import Model.Formula as Formula exposing (..)
import Model.Flower exposing (..)
import Model.App exposing (..)

import Update.Rules exposing (..)
import Update.App exposing (..)

import Utils.List
import Utils.Events
import Utils.Color

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input

import Html.Attributes exposing (title)

import Html5.DragDrop as DnD

import Css

import FeatherIcons as Icons

import Color
import Utils.Events exposing (onClick)
import View.Widgets as Widgets


reorderColor : Color.Color
reorderColor =
  Utils.Color.fromRgb { red = 0.7, green = 0.7, blue = 0.7 }


importColor : Color.Color
importColor =
  Utils.Color.fromRgb { red = 1, green = 0.8, blue = 0 }


cropAction : Context -> Flower -> List (Attribute Msg)
cropAction context flower =
  let actionableStyle = redActionable in
  if croppable context || isGrownFlower flower then
    Utils.Events.onClick (Action Crop context.zipper [flower])
    :: (htmlAttribute <| title "Remove Flower")
    :: actionableStyle.active
  else
    actionableStyle.inactive


pullAction : Context -> Garden -> List (Attribute Msg)
pullAction context (Garden gardenData) =
  let actionableStyle = redActionable in
  if pullable context then
    Utils.Events.onClick (Action Pull context.zipper gardenData.flowers)
    :: (htmlAttribute <| title "Remove Petal")
    :: actionableStyle.active
  else
    actionableStyle.inactive


drawGrownBorder : Bool -> List (Attribute msg)
drawGrownBorder doit =
  if doit then grownBorder.active else grownBorder.inactive


viewFormula : Model -> Context -> FormulaData -> Element Msg
viewFormula model context ({ metadata, statement } as data) =
  let
    form =
      Formula data

    clickAction =
      let
        actionableStyle =
          case statement of
            Atom _ -> greenActionable
            _ -> pinkActionable
      in
      case model.mode of
        ProofMode Justifying ->
          case statement of
            Atom _ ->
              if isHypothesis form context.zipper then
                (Events.onClick (Action Justify context.zipper [form]))
                :: (htmlAttribute <| title "Justify")
                :: actionableStyle.active
              else
                actionableStyle.inactive
            
            _ ->
              (Events.onClick (Action Decompose context.zipper [form]))
              :: (htmlAttribute <| title "Decompose")
              :: actionableStyle.active
        
        EditMode Operating _ ->
          cropAction context (Formula data)

        _ ->
          actionableStyle.inactive
    
    reorderDragAction =
      case model.mode of
        EditMode _ _ ->
          dragAction reorderColor model.dragDrop context.zipper form
        
        _ ->
          []
  in
  el
    ( [ centerX, centerY
      , padding 10
      , Font.color (flowerForegroundColor context.polarity)
      , Font.size 50
      , nonSelectable ]
      ++ reorderDragAction
      ++ clickAction
      ++ drawGrownBorder metadata.grown )
    ( text (Formula.toString statement) )


viewPistil : Model -> Context -> PistilData -> Garden -> Element Msg
viewPistil model context { metadata, pistilMetadata, petals } pistil =
  let
    newZipper =
      mkPistil metadata pistilMetadata petals :: context.zipper

    clickAction =
      case model.mode of
        ProofMode Justifying ->
          let
            actionableStyle = orangeActionable

            action rule name =
              (Events.onClick (Action rule newZipper (harvest pistil)))
              :: (htmlAttribute <| title name)
              :: actionableStyle.active
          in
          if List.isEmpty (harvest pistil) then
            case context.zipper of
              _ :: Pistil _ :: _ ->
                let
                  (rule, name) =
                    case List.length petals of
                      0 -> (Close, "Ex falso quodlibet")
                      1 -> (Unlock, "Unlock")
                      _ -> (Case, "Case")
                in
                action rule name
              _ ->
                if List.length petals == 1 then
                  action Unlock "Unlock"
                else
                  actionableStyle.inactive
          else
            actionableStyle.inactive
        
        EditMode Operating _ ->
          cropAction context (mkFlower metadata pistil petals)

        _ ->
          (actionable Utils.Color.transparent).inactive
  in
  el
    ( [ width fill
      , height fill
      , Border.rounded flowerBorderRound ] )
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


viewPetal : Model -> Context -> PetalData -> Garden -> Element Msg
viewPetal model context { metadata, petalMetadata, pistil, left, right } (Garden petalData as petal) =
  let
    newZipper =
      mkPetal metadata petalMetadata pistil left right :: context.zipper

    clickAction =
      let actionableStyle = greenActionable in
      case model.mode of
        ProofMode Justifying ->
          if List.isEmpty petalData.flowers then
            (Events.onClick (Action Close newZipper petalData.flowers))
            :: (htmlAttribute <| title "QED")
            :: actionableStyle.active
          else
            actionableStyle.inactive
        
        EditMode Operating _ ->
          pullAction { context | zipper = newZipper } petal

        _ ->
          actionableStyle.inactive
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


addButton : ButtonParams msg -> Element msg
addButton params =
  let
    style =
      { width = Css.pct 100
      , height = Css.pct 100
      , color = Color.rgb255 58 134 255
      , iconColorEnabled = Color.white
      , iconColorDisabled = Color.darkGray }
  in
  button style params


viewAddPetalZone : Context -> FlowerData -> Element Msg
viewAddPetalZone context { metadata, pistil, petals } =
  let
    newFlower =
      mkFlower metadata pistil (petals ++ [mkFakeGarden []])

    addPetalButton =
        ( addButton
            { action =  Msg (Action Grow context.zipper [newFlower])
            , title = "Add Petal"
            , icon = Icons.plus
            , enabled = True } )
  in
  column
    [ width shrink
    , height fill
    , padding 10
    , Border.rounded flowerBorderRound
    , Background.color (flowerBackgroundColor (invert context.polarity)) ]
    [ addPetalButton ]


viewAddFlowerZone : Context -> String -> Bouquet -> Element Msg
viewAddFlowerZone context newAtomName flowers =
  let
    newFlower =
      if String.isEmpty newAtomName then
        mkFakeFlower (mkFakeGarden []) [mkFakeGarden []]
      else
        mkFakeFormula (Atom newAtomName)

    newZipper newName =
      case context.zipper of
        [] -> [mkBouquet flowers []]
        zip :: zipper ->
          case zip of
            Bouquet { left, right } ->
              mkBouquet (left ++ flowers) right :: zipper
            Pistil ({ pistilMetadata } as data) ->
              mkBouquet flowers [] ::
              Pistil { data | pistilMetadata = { pistilMetadata | newAtomName = newName } } ::
              zipper
            Petal ({ petalMetadata } as data) ->
              mkBouquet flowers [] ::
              Petal { data | petalMetadata = { petalMetadata | newAtomName = newName } } ::
              zipper
    
    onChange newName =
      SetGoal (fillZipper [] (newZipper newName))
    
    addAtomTextEdit =
      Input.text
        [ width (105 |> px)
        , Border.rounded flowerBorderRound
        , onClick DoNothing ]
        { onChange = onChange
        , text = newAtomName
        , placeholder = Just (Input.placeholder [] (text "flower"))
        , label = Input.labelHidden "Atom name" }

    addFlowerButton =
      el
        [ width fill
        , height fill ]
        ( addButton
            { action = Msg (Action Grow (newZipper newAtomName) [newFlower])
            , title = "Add Flower"
            , icon = Icons.plus
            , enabled = True } )
  in
  column
    [ width shrink
    , height fill
    , centerX
    , Border.rounded flowerBorderRound
    , Background.color Style.transparent ]
    [ addAtomTextEdit
    , addFlowerButton ]


viewFlower : Model -> Context -> Flower -> Element Msg
viewFlower model context flower =
  case flower of
    Formula formula ->
      viewFormula model context formula
    
    Flower ({ metadata, pistil, petals } as data) ->
      let
        (Garden pistilData) = pistil

        pistilEl =
          viewPistil model context (PistilData metadata pistilData.metadata petals) pistil
        
        addPetalZone =
          case model.mode of 
            EditMode _ _ ->
              if glueable context || data.metadata.grown then
                [viewAddPetalZone context data]
              else
                []
            _ ->
              []
        
        petalsEl =
          row
            [ width fill
            , height fill
            , spacing flowerBorderWidth ]
            ( Utils.List.zipperMap
                (\(left, right) (Garden petalData as petal) ->
                  viewPetal model context (PetalData metadata petalData.metadata pistil left right) petal)
                petals
              ++ addPetalZone )

        
        color =
          case model.mode of
            ProofMode _ -> importColor
            EditMode _ _ -> reorderColor
            _ -> Utils.Color.transparent
      in
      column
        ( [ width fill
          , height fill
          , Background.color (flowerForegroundColor context.polarity) ]
         ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
         ++ dragAction color model.dragDrop context.zipper flower
         ++ onClick DoNothing
         :: Border.solid
         :: Border.width grownBorder.borderWidth
         :: drawGrownBorder metadata.grown
         ++
          [ Border.shadow
              { offset = (0, 5)
              , size = 0.25
              , blur = 15
              , color = flowerForegroundColor context.polarity } ] )
        [ pistilEl, petalsEl ]


viewBouquet : Model -> Context -> String -> Bouquet -> Element Msg
viewBouquet model context newAtomName bouquet =
  let
    flowerEl (left, right) =
      viewFlower
        model
        { context
        | zipper = mkBouquet left right :: context.zipper }
    
    dropAction (left, right) =
      case model.mode of
        ProofMode Importing ->
          case DnD.getDragId model.dragDrop of
            Just { source } ->
              -- if isHypothesis content context.zipper then
              if justifies source context.zipper then
                let
                  dropStyle =
                    droppable importColor

                  dropTargetStyle =
                    case DnD.getDropId model.dragDrop of
                      Just (Just { target }) ->
                        if mkBouquet left right :: context.zipper == target
                        then dropStyle.active
                        else dropStyle.inactive
                    
                      _ ->
                        dropStyle.inactive
                in
                dropTargetStyle ++
                ( List.map htmlAttribute <|
                  DnD.droppable DragDropMsg
                    (Just { target = mkBouquet left right :: context.zipper
                          , content = [] }) )
              else
                []
            _ ->
              []

        EditMode Reordering _ ->
          case DnD.getDragId model.dragDrop of
            Just { source, content } ->
              case source of
                Bouquet sourceBouquet :: sourceParent ->
                  if sourceParent == context.zipper then
                    let
                      (sourceIndex, index) =
                        (List.length sourceBouquet.left, List.length left)
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
                            sourceBouquet.left ++ middle ++ content :: right
                          else
                            let
                              middle =
                                Utils.List.slice index (sourceIndex - 1) whole
                            in
                            left ++ content :: (middle ++ sourceBouquet.right)

                        dropStyle =
                          droppable reorderColor

                        dropTargetStyle =
                          case DnD.getDropId model.dragDrop of
                            Just (Just { target }) ->
                              if mkBouquet left right :: context.zipper == target
                              then dropStyle.active
                              else dropStyle.inactive

                            _ ->
                              dropStyle.inactive
                      in
                      dropTargetStyle ++
                      ( List.map htmlAttribute <|
                        DnD.droppable DragDropMsg
                          (Just { target = mkBouquet left right :: context.zipper
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
      [ Border.width (droppable Utils.Color.transparent).borderWidth
      , Border.color Style.transparent ]

    length flower =
      case flower of
        Formula _ -> shrink
        Flower _ -> fill

    intersticial () =
      let
        attrs =
          layoutAttrs

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
        
        addFlowerZone =
          case model.mode of 
            EditMode _ _ ->
              if growable context then
                [viewAddFlowerZone context newAtomName bouquet]
              else
                []
            _ ->
              []
      in
      wrappedRow attrs (els ++ addFlowerZone)
        
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
    EditMode _ _ ->
      intersticial ()

    _ ->
      normal ()


viewGarden : Model -> Context -> Garden -> Element Msg
viewGarden model context (Garden { metadata, flowers }) =
  el
    ( fillXY ++
      drawGrownBorder metadata.grown )
    ( viewBouquet model context metadata.newAtomName flowers )


viewGoal : Model -> Element Msg
viewGoal model =
  let
    goalEl () =
      el
        ( scrollbars ::
          fillXY )
        ( viewBouquet model (Context [] Pos) "" model.goal )
  in
  case model.mode of
    ProofMode _ ->
      goalEl ()
    
    EditMode _ _ ->
      goalEl ()

    _ ->
      Widgets.fullPageTextMessage "Working on it!"