(* interpreter.ml *)

open Absyn


let get_type_id (type', id) =
  match type' with
  | Absyn.Int -> (id, Absyn.Int)
  | Absyn.Bool -> (id, Absyn.Bool)

(* let interpreter (_, y) = 
  call_fun y *)
