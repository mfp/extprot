
open Ptypes
open ExtList
open Printf

let failwithfmt fmt = kprintf (fun s -> if true then failwith s) fmt

let low_level_msg_def bindings (msg : message_expr) =

  let update_bindings bindings params args =
    List.fold_right2 SMap.add params args bindings in

  let unbound_tvar s =
    failwithfmt "expand_type_defs: unbound type variable %S" s; assert false in

  let rec beta_reduction bindings : type_expr -> reduced_type_expr = function
    | #base_type_expr_simple as x -> x
    | `Sum s ->
        let non_const =
          List.map
            (fun (const, tys) ->
               (const, List.map (beta_reduction bindings) (tys :> type_expr list)))
            s.non_constant
        in `Sum { s with non_constant = non_const }
    | `Tuple l -> `Tuple (List.map (beta_reduction bindings) (l :> type_expr list))
    | `List t -> `List (beta_reduction bindings (type_expr t))
    | `Array t -> `Array (beta_reduction bindings (type_expr t))
    | `App (name, args) -> begin match smap_find name bindings with
          Some (Message_decl _) -> `Message name
        | Some (Type_decl (name, params, exp)) ->
            let bindings =
              update_bindings bindings params
                (List.map (fun ty -> Type_decl ("<bogus>", [], type_expr ty)) args)
            in beta_reduction bindings exp
        | None -> unbound_tvar name
      end in

  let rec low_level_of_rtexp : [reduced_type_expr] -> low_level = function
      `Bool -> Vint Bool
    | `Byte -> Vint Positive_int
    | `Int true -> Vint Positive_int
    | `Int false -> Vint Int
    | `Long_int -> Bitstring64 Long
    | `Float -> Bitstring64 Float
    | `String -> Bytes
    | `Tuple l -> Tuple (List.map low_level_of_rtexp (l :> reduced_type_expr list))
    | `List ty -> Htuple (List, low_level_of_rtexp (reduced_type_expr ty))
    | `Array ty -> Htuple (Array, low_level_of_rtexp (reduced_type_expr ty))
    | `Message s -> Message s
    | `Sum sum ->
        let constant =
          List.mapi
            (fun i s -> { const_tag = i; const_name = s; const_type = sum.type_name })
            sum.constant in
        let non_constant =
          List.mapi
            (fun i (const, tys) ->
               ({ const_tag = i; const_name = const; const_type = sum.type_name},
                List.map low_level_of_rtexp tys))
            sum.non_constant
        in Sum (constant, non_constant) in

  let rec low_level_of_mexpr : message_expr -> low_level_record =
    let low_level_field (const, mutabl, ty) =
      (const, mutabl, low_level_of_rtexp (beta_reduction bindings (type_expr ty)))

    in function
      `Sum cases  ->
        Record_sum
          (List.map
             (fun (const, `Record fields) -> (const, List.map low_level_field fields))
             cases)
    | `Record fields -> Record_single (List.map low_level_field fields)

  in low_level_of_mexpr msg

let collect_bindings =
  List.fold_left (fun m decl -> SMap.add (declaration_name decl) decl m) SMap.empty

