module View.Goal exposing (..)

import View.Style as Style exposing (..)
import View.Events exposing (..)

import Model.Formula as Formula exposing (..)
import Model.Flower exposing (..)
import Model.App exposing (..)

import Update.App exposing (..)

import Utils.List
import Utils.Color as Color

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font

import Html.Attributes exposing (title)

import Html5.DragDrop as DnD


reorderColor : Color.Color
reorderColor =
  Color.fromRgb { red = 0.7, green = 0.7, blue = 0.7 }


importColor : Color.Color
importColor =
  Color.fromRgb { red = 1, green = 0.8, blue = 0 }


viewFormula : Model -> Context -> Formula -> Element Msg
viewFormula model context formula =
  let
    form =
      Formula formula

    clickAction =
      let
        actionableStyle =
          case formula of
            Atom _ -> greenActionable
            _ -> pinkActionable
      in
      case model.mode of
        ProofMode Justifying ->
          case formula of
            Atom _ ->
              if isHypothesis form context.zipper then
                (Events.onClick (Action Justify [form] context.zipper))
                :: (htmlAttribute <| title "Justify")
                :: actionableStyle.active
              else
                actionableStyle.inactive
            
            _ ->
              (Events.onClick (Action Decompose [form] context.zipper))
                :: (htmlAttribute <| title "Decompose")
              :: actionableStyle.active

        _ ->
          actionableStyle.inactive
    
    reorderDragAction =
      case model.mode of
        EditMode _ ->
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
      ++ clickAction
      ++ reorderDragAction )
    ( text (Formula.toString formula) )


viewPistil : Model -> Context -> Garden -> List Garden -> Element Msg
viewPistil model context (Garden bouquet as pistil) petals =
  let
    newZipper =
      Pistil petals :: context.zipper

    clickAction =
      let actionableStyle = orangeActionable in
      case model.mode of
        ProofMode Justifying ->
          let
            action name =
              (Events.onClick (Action Unlock bouquet newZipper))
              :: (htmlAttribute <| title name)
              :: actionableStyle.active
          in
          if List.isEmpty bouquet then
            case context.zipper of
              _ :: Pistil _ :: _ ->
                let
                  name =
                    case List.length petals of
                      0 -> "Ex falso quodlibet"
                      1 -> "Unlock"
                      _ -> "Case"
                in
                action name
              _ ->
                if List.length petals == 1 then
                  action "Unlock"
                else
                  actionableStyle.inactive
          else
            actionableStyle.inactive
        
        _ ->
          actionableStyle.inactive
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


viewPetal : Model -> Context -> Garden -> (List Garden, List Garden) -> Garden -> Element Msg
viewPetal model context pistil (leftPetals, rightPetals) (Garden bouquet as petal) =
  let
    newZipper =
      Petal pistil leftPetals rightPetals :: context.zipper

    clickAction =
      let actionableStyle = greenActionable in
      case model.mode of
        ProofMode Justifying ->
          if List.isEmpty bouquet then
            (Events.onClick (Action Close bouquet newZipper))
            :: (htmlAttribute <| title "QED")
            :: actionableStyle.active
          else
            actionableStyle.inactive
        
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


viewFlower : Model -> Context -> Flower -> Element Msg
viewFlower model context flower =
  case flower of
    Formula formula ->
      viewFormula model context formula
    
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
        
        color =
          case model.mode of
            ProofMode _ -> importColor
            EditMode _ -> reorderColor
            _ -> Color.transparent
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
        ++ dragAction color model.dragDrop context.zipper flower )
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
                    droppable importColor

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
                          droppable reorderColor

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
      , Border.color Style.transparent ]

    length flower =
      case flower of
        Formula _ -> shrink
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