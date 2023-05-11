module Model.Goal exposing (..)


import Model.Flower exposing (..)


-- The goal is either solved, or a flower to prove


type Goal
  = QED
  | Prove Flower


fromBouquet : Bouquet -> Goal
fromBouquet bouquet =
  case bouquet of
    [flower] -> Prove flower
    [] -> QED
    _ ->
      let _ = Debug.log "" bouquet in
      Debug.todo "Not a goal"


map : (Bouquet -> Bouquet) -> Goal -> Goal
map f goal =
  case goal of
    QED -> QED
    Prove x ->
      case f [x] of
        [flower] -> Prove flower
        [] -> QED
        _ -> Debug.todo "Not a goal"