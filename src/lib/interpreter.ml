(* interpreter.ml *)

open Absyn

(* type checking of expressions *)

let rec check_exp (exp, vtable, ftable) =
  match exp with
  | (_, Absyn.IntExp _) -> Absyn.Int
  | (_, Absyn.VarExp x) -> 
    (match Symbol.look x vtable with
    | Some var -> var
    | None -> Error.error (Location.loc exp) "debug var(id)")
  | (_, Absyn.OpExp (Absyn.Plus, left, right)) ->
    let t1 = check_exp (left, vtable, ftable) in
    let t2 = check_exp (right, vtable, ftable) in
    if t1 == Absyn.Int && t2 == Absyn.Int
      then Absyn.Int
    else 
      Error.error (Location.loc exp) "debug plus"
      Absyn.Int
  | (_, Absyn.OpExp (Absyn.LT, left, right)) -> 
    let t1 = check_exp (left, vtable, ftable) in
    let t2 = check_exp (right, vtable, ftable) in
    if t1 == t2
      then Absyn.Bool
    else 
      Error.error (Location.loc exp) "debug lt"
      Absyn.Bool
  | (_, Absyn.IfExp (if', then', else')) -> 
    let t1 = check_exp (if', vtable, ftable) in
    let t2 = check_exp (then', vtable, ftable) in
    let t3 = check_exp (else', vtable, ftable) in
    if t1 == Absyn.Bool && t2 == t3
      then t2
    else 
      Error.error (Location.loc exp) "debug if"
      t2
  | (_, Absyn.CallExp (id, exps)) -> 
    let _ = Symbol.look id ftable in
    (match Symbol.look id ftable with
      | Some exp_list' ->
        let param_length = List.length exp_list' in
        let verify_exps = check_exps (exps, vtable, ftable) in
        let verified_exps_length = List.length verify_exps in
        if cmp verify_exps exp_list' &&
           verified_exps_length == param_length 
              then (match Symbol.look id vtable with
                    | Some t0 -> t0
                    | None -> Error.error (Location.loc exp) "debug var(id)")
        else      
          Error.error (Location.loc exp) "debug callexp"
          id
      | None -> Error.error (Location.loc exp) "debug callexp unbound"
        Absyn.Int
    )
  | (_, Absyn.LetExp (id, id_exp, in_exp)) -> 
    let t1 = check_exp(id_exp, vtable, ftable) in
    let new_vtable = Symbol.enter id t1 vtable in
      check_exp (in_exp, new_vtable, ftable)
  
and check_exps (exps, vtable, ftable) =
  match exps with
  | [exp] -> [check_exp (exp, vtable, ftable)]
  | exp :: tail -> check_exp (exp, vtable, ftable) 
      :: check_exps (tail, vtable, ftable)
  | [] -> []

and cmp list1 list2 =
  match (list1, list2) with
  | [a], [b] -> a == b
  | (a :: tail1), (b :: tail2) ->
      a == b && cmp tail1 tail2
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false

let get_type_id (type', id) =
  match type' with
  | Absyn.Int -> (id, Absyn.Int)
  | Absyn.Bool -> (id, Absyn.Bool)
(* let interpreter (_, y) = 
  call_fun y *)