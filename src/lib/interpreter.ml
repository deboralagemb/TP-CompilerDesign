(* interpreter.ml *)

open Absyn


(* type checking of expressions *)

let rec check_exp (exp, vtable, ftable) =
  match exp with
  | (_, Absyn.IntExp _) -> Absyn.Int
  | (_, Absyn.VarExp x) -> 
    (match Symbol.look x vtable with
    | Some var -> var
    | None -> Error.error (Location.loc exp) "debug1")
  | (_, Absyn.OpExp (Absyn.Plus, left, right)) ->
    let t1 = check_exp (left, vtable, ftable) in
    let t2 = check_exp (right, vtable, ftable) in
    if t1 == Absyn.Int && t2 == Absyn.Int
      then Absyn.Bool
    else 
      Error.error (Location.loc exp) "debug2"
      Absyn.Bool
  | (_, Absyn.OpExp (Absyn.LT, y, z)) -> Absyn.Int
  | (_, Absyn.IfExp (x, y, z)) -> Absyn.Int
  | (_, Absyn.CallExp (x, y)) -> Absyn.Int
  | (_, Absyn.LetExp (x, y, z)) -> Absyn.Int
  



let get_type_id (type', id) =
  match type' with
  | Absyn.Int -> (id, Absyn.Int)
  | Absyn.Bool -> (id, Absyn.Bool)
(* let interpreter (_, y) = 
  call_fun y *)