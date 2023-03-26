module Update.Rules exposing (..)

import Utils.List exposing (forall, zipperFoldl)

import Model.Formula exposing (..)
import Model.Flower exposing (..)
import Model.App exposing (..)


type Rule
  = Decompose -- introduction of connective
  | Justify -- down pollination
  | Import -- up pollination
  | Unlock -- empty pistil with single petal
  | Case -- empty pistil with many petals
  | Close -- empty petal / empty pistil with no petal
  | Fence -- fencing
  | Reorder -- multiset
  | Grow -- add flower
  | Glue -- add petal
  | Crop -- remove flower
  | Pull -- remove petal


decomposable : Zipper -> Bouquet -> Bool
decomposable _ bouquet =
  case bouquet of
    [Formula (Atom _)] -> False
    [Formula _] -> True
    _ -> False


justifiable : Zipper -> Bouquet -> Bool
justifiable zipper bouquet =
  let
    justifiableFlower flower =
      case flower of
        Formula (Atom _) ->
          isHypothesis flower zipper
        _ ->
          False
  in
  not (List.isEmpty bouquet) &&
  forall justifiableFlower bouquet


unlockable : Zipper -> Bouquet -> Bool
unlockable zipper bouquet =
  case (bouquet, zipper) of
    ([], Pistil [Garden _] :: _) -> True
    _ -> False


caseable : Zipper -> Bouquet -> Bool
caseable zipper bouquet =
  case (bouquet, zipper) of
    ([], Pistil petals :: Bouquet _ _ :: Pistil _ :: _) ->
      List.length petals > 1
    _ ->
      False


closeable : Zipper -> Bouquet -> Bool
closeable zipper bouquet =
  case (bouquet, zipper) of
    ([], Pistil [] :: Bouquet _ _ :: Pistil _ :: _ ) -> True
    ([], Petal _ _ _ :: _) -> True
    _ -> False


autoRules : Zipper -> Bouquet -> List Rule
autoRules zipper bouquet =
  let
    rulePreds =
      [ (Decompose, decomposable)
      , (Justify, justifiable)
      , (Unlock, unlockable)
      , (Case, caseable)
      , (Close, closeable) ]
  in
  List.foldl
    (\(rule, pred) acc ->
      if pred zipper bouquet then rule :: acc else acc)
    [] rulePreds


operable : Polarity -> Surgery -> Context -> Bool
operable polarity surgery context =
  context.polarity == polarity ||
  Utils.List.hasSuffix
    (\ancestor -> List.member ancestor surgery.growing)
    context.zipper


growable : Surgery -> Context -> Bool
growable =
  operable Pos


glueable : Surgery -> Context -> Bool
glueable =
  operable Neg


pullable : Surgery -> Context -> Bool
pullable =
  operable Pos


croppable : Surgery -> Context -> Bool
croppable =
  operable Neg


operate : Rule -> Zipper -> Bouquet -> Surgery -> Surgery
operate rule zipper bouquet surgery =
  case rule of
    Grow ->
      { surgery | growing = zipper :: surgery.growing }
    
    _ ->
      surgery


apply : Rule -> Zipper -> Bouquet -> Bouquet
apply rule zipper bouquet =
  case (rule, bouquet, zipper) of
    (Decompose, [Formula formula], _) ->
      fillZipper (decompose formula) zipper

    (Justify, _, _) ->
      fillZipper [] zipper
    
    (Import, _, _) ->
      fillZipper bouquet zipper

    (Unlock, [], Pistil [Garden petal] :: parent)  ->
      fillZipper petal parent
    
    (Case, [], Pistil branches :: Bouquet left right :: Pistil petals :: parent) ->
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
    
    (Close, [], Pistil [] :: Bouquet _ _ :: Pistil _ :: parent ) ->
      fillZipper [] parent
    
    (Close, [], Petal _ _ _ :: parent) ->
      fillZipper [] parent
    
    (Reorder, _, _ :: parent) ->
      fillZipper bouquet parent
    
    (Grow, _, _) ->
      fillZipper bouquet zipper
    
    (Glue, _, _) ->
      fillZipper bouquet zipper
    
    (Crop, _, _) ->
      fillZipper bouquet zipper

    (Pull, _, _) ->
      fillZipper bouquet zipper

    _ ->
      Debug.todo "Unsupported action"


tryRules : List Rule -> List Rule -> Zipper -> Bouquet -> Maybe Bouquet
tryRules candidates allowed zipper bouquet =
  case candidates of
    [] ->
      Nothing
    rule :: rules ->
      if not (List.member rule allowed) then
        tryRules rules allowed zipper bouquet
      else
        Just (apply rule zipper bouquet)


autoFlower : List Rule -> Zipper -> Flower -> Maybe Bouquet
autoFlower allowed zipper flower =
  case flower of
    Formula _ ->
      let candidates = autoRules zipper [flower] in
      tryRules candidates allowed zipper [flower]
    
    Flower pistil petals ->
      -- First try on pistil
      let resultPistil = autoGarden allowed (Pistil petals :: zipper) pistil in
      case resultPistil of
        Just _ -> resultPistil
        Nothing ->
          -- Then try on petals
          let
            autoPetal (left, right) petal acc =
              case acc of
                Just _ -> acc
                Nothing ->
                  autoGarden allowed (Petal pistil left right :: zipper) petal
          in
          zipperFoldl autoPetal Nothing petals


autoGarden : List Rule -> Zipper -> Garden -> Maybe Bouquet
autoGarden allowed zipper (Garden bouquet) =
  autoBouquet allowed zipper bouquet


autoBouquet : List Rule -> Zipper -> Bouquet -> Maybe Bouquet
autoBouquet allowed zipper bouquet =
  -- First try on the whole bouquet
  let
    candidates = autoRules zipper bouquet
    wholeResult = tryRules candidates allowed zipper bouquet
  in
  case wholeResult of
    Just _ -> wholeResult
    Nothing ->
      -- Then try on each flower of the bouquet
      let
        -- Let's assume flowers in a bouquet are always tulips, shall we?
        autoTulip (left, right) tulip acc =
          case acc of
            Just _ -> acc
            Nothing ->
              autoFlower allowed (Bouquet left right :: zipper) tulip
      in
      zipperFoldl autoTulip Nothing bouquet


auto : List Rule -> Bouquet -> Bouquet
auto allowed bouquet =
  case autoBouquet allowed [] bouquet of
    Just result ->
      auto allowed result
    Nothing ->
      bouquet