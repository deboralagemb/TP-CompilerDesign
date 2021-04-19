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
  | (_, Absyn.IfExp (x, y, z)) -> Absyn.Int
  | (_, Absyn.OpExp (x, y, z)) -> Absyn.Int
  | (_, Absyn.CallExp (x, y)) -> Absyn.Int
  | (_, Absyn.LetExp (x, y, z)) -> Absyn.Int
  



let get_type_id (type', id) =
  match type' with
  | Absyn.Int -> (id, Absyn.Int)
  | Absyn.Bool -> (id, Absyn.Bool)
(* let interpreter (_, y) = 
  call_fun y *)