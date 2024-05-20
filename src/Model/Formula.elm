module Model.Formula exposing (..)


type Ident
  = Name String
  | Image { src : String, description : String }

type Formula
  = Atom Ident
  | Truth | Falsity
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  | Not Formula


atom : String -> Formula
atom name =
  Atom (Name name)


toString : Formula -> String
toString formula =
  case formula of
    Atom (Name name) ->
      name

    Atom (Image { description }) ->
      description
    
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
    
    Not f1 ->
      "¬ (" ++ toString f1 ++ ")"