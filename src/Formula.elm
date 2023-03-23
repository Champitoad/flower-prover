module Formula exposing (..)


type Formula
  = Atom String
  | Truth | Falsity
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula


toString : Formula -> String
toString formula =
  case formula of
    Atom name ->
      name
    
    Truth ->
      "⊤"
    
    Falsity ->
      "⊥"
    
    And f1 f2 ->
      toString f1 ++ " ∧ " ++ toString f2

    Or f1 f2 ->
      toString f1 ++ " ∨ " ++ toString f2

    Implies f1 f2 ->
      "(" ++ toString f1 ++ " ⇒ " ++ toString f2 ++ ")"