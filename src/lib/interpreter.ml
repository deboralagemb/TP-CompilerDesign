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

(* type checking a function declaration *)
let rec check_fun ((typeid, typeids, exp), ftable) =
  let (_, t0) = get_type_id (typeid) in
  let vtable = check_type_ids (typeids, exp) in
  let t1 = check_exp (exp, vtable, ftable) in
  if t0 != t1
    then Error.error (Location.loc exp) "debug check funs"


and get_type_id (type', id) =
  match type' with
  | Absyn.Int -> (id, Absyn.Int)
  | Absyn.Bool -> (id, Absyn.Bool)

and check_type_ids (typeids, exp) =
  match typeids with
  | [typeid] -> let (x, t) = get_type_id (typeid) in
                Symbol.enter x t Symbol.empty
  | typeid :: tail -> let (x, t) = get_type_id (typeid) in
                      let vtable = check_type_ids (tail, exp) in
                      match Symbol.look x vtable with
                      | Some _ -> Error.error (Location.loc exp) "verifiy this exp <<-"
                      | None -> Symbol.enter x t vtable


(* type checking a program *)

let rec check_funs (funs, ftable) =
  match funs with
  | [fun'] -> check_fun(fun', ftable)
  | fun' :: tail -> check_fun(fun', ftable);
                    check_funs(tail, ftable)

let rec get_types (type_ids) =
  match type_ids with
  | [type_id] -> let (_, t) = get_type_id (type_id) in
                 [t]
  | type_id :: tail -> let (_, t) = get_type_id (type_id) in
                       let tail_ = get_types (tail) in
                       t :: tail_

let get_fun (fun') =
  match fun' with
  | (typeId, type_ids, exp) -> let (f, t0) = get_type_id (typeId) in
                               let types = get_types (type_ids) in
                               (f, types, t0)

let rec get_funs (funs) =
  match funs with
  | [fun'] -> let (f, _, t) = get_fun(fun') in
              Symbol.enter f t Symbol.empty
  | fun' :: tail -> let (f, _, t) = get_fun(fun') in
                    let ftable = get_funs(tail) in
                    match Symbol.look f ftable with
                    | None -> Symbol.enter f t ftable 
                    | Some ftable -> Error.error (Location.loc f) "debug funs" ftable


(* let check_program (program) =
  let ftable = get_funs(program) in
  let _ = check_funs(program, ftable) in

*)