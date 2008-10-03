open Camlp4.PreCast
open Printf
open Syntax

type tag = int

type low_level =
    Vint of vint_meaning
  | Bitstring32
  | Bitstring64 of b64_meaning
  | Bytes
  | Sum of (tag * string) list * (tag * string * low_level list) list
  | Tuple of low_level list
  | Htuple of htuple_meaning * low_level
  | Message of string

and low_level_record =
  | Record_single of (string * bool * low_level) list
  | Record_sum of (string * (string * bool * low_level) list) list

and vint_meaning =
    Bool
  | Positive_int
  | Int

and b64_meaning =
    Long
  | Float

and htuple_meaning =
    List
  | Array

type base_type_expr_simple = [
    `Bool
  | `Byte
  | `Int of bool      (* true if positive *)
  | `Long_int
  | `Float
  | `String
]

type 'a base_type_expr_core = [
    base_type_expr_simple
  | `Tuple of 'a list
  | `List of 'a
  | `Array of 'a
]

type base_type_expr = [
    base_type_expr base_type_expr_core
  | `App of string * base_type_expr list
]

type type_expr = [
    base_type_expr
  (* | `Record of (string * bool * base_type_expr) list *)
  | `Sum of base_type_expr sum_data_type
]

and reduced_type_expr = [
    reduced_type_expr base_type_expr_core
  | `Sum of reduced_type_expr sum_data_type
  | `Message of string
]

and 'a sum_data_type = {
  constant : string list;
  non_constant : (string * 'a list) list
}

type base_message_expr = [ `Record of (string * bool * base_type_expr) list ]

type message_expr = [ base_message_expr | `Sum of (string * base_message_expr) list ]

type declaration =
    Message_decl of string * message_expr
  | Type_decl of string * string list * type_expr

let type_expr e = (e :> type_expr)
let reduced_type_expr e = (e :> reduced_type_expr)

let declaration_name = function Message_decl (n, _) | Type_decl (n, _, _) -> n

let declaration_arity = function
    Message_decl _ -> 0
  | Type_decl (_, l, _) -> List.length l

module SSet = Set.Make(struct type t = string let compare = String.compare end)
module SMap = Map.Make(struct type t = string let compare = String.compare end)

let smap_find k m = try Some (SMap.find k m) with Not_found -> None

type error =
    Repeated_binding of string
  | Unbound_type_variable of string * string
  | Wrong_arity of string * int * string * int

let concat_map f l = List.concat (List.map f l)

let free_type_variables decl : string list =
  let rec free_vars known : base_type_expr -> string list = function
    | `App (n, tys) ->
        let l = concat_map (free_vars known) tys in
          if List.mem n known then l else n :: l
    | `Tuple l -> concat_map (free_vars known) l
    | `List t | `Array t -> free_vars known t
    | #base_type_expr_simple -> [] in

  let rec type_free_vars known : type_expr -> string list = function
      #base_type_expr as x -> free_vars known x
    | `Sum sum ->
        concat_map
          (fun (_, l) -> concat_map (type_free_vars known) (l :> type_expr list))
          sum.non_constant in

  let rec msg_free_vars known = function
      `Record l ->
        concat_map (fun (_, _, e) -> type_free_vars known (e :> type_expr)) l
    | `Sum l ->
        concat_map (fun (_, e) -> msg_free_vars known (e :> message_expr)) l in

  match decl with
      Message_decl (name, m) -> msg_free_vars [name] m
    | Type_decl (name, tvars, e) -> type_free_vars (name :: tvars) e

let check_declarations decls =

  let dup_errors l =
    let rec loop errs bindings = function
        [] -> errs
      | decl :: tl ->
          let name = declaration_name decl in
            if SSet.mem name bindings then
              loop (Repeated_binding name :: errs) bindings tl
            else
              loop errs (SSet.add name bindings) tl
    in loop [] SSet.empty l in

  let unbound_type_vars l =
    let rec loop errs bindings = function
        [] -> errs
      | decl :: tl ->
          let name = declaration_name decl in
          let errs = List.fold_right
                       (fun n l ->
                          if SSet.mem n bindings then l
                          else (Unbound_type_variable (name, n) :: l))
                       (free_type_variables decl)
                       errs
          in loop errs (SSet.add (declaration_name decl) bindings) tl
    in loop [] SSet.empty l in

  let wrong_type_arities l =
    let rec loop errs arities = function
        [] -> errs
      | decl :: tl ->
          let name = declaration_name decl in
          let arities = SMap.add name (declaration_arity decl) arities in

          let rec fold_base_ty acc : base_type_expr -> error list = function
              #base_type_expr_simple -> acc
            | `Tuple l -> List.fold_left fold_base_ty acc l
            | `List t | `Array t -> fold_base_ty acc t
            | `App (s, params) ->
                let expected = List.length params in
                  begin match smap_find s arities with
                      None -> acc
                    | Some n when n = expected -> acc
                    | Some n -> Wrong_arity (s, n, name, expected) :: acc
                  end in

          let rec fold_msg acc : message_expr -> error list = function
              `Record l ->
                List.fold_left (fun errs (_, _, ty) -> fold_base_ty errs ty) acc l
            | `Sum l ->
                List.fold_left
                  (fun errs (_, msg) -> fold_msg errs (msg :> message_expr))
                  acc l in

          let fold_ty acc : type_expr -> error list = function
              #base_type_expr as bty -> fold_base_ty acc bty
            | `Sum sum ->
                List.fold_left
                  (fun acc (_, l) -> List.fold_left fold_base_ty acc l)
                  acc sum.non_constant

          in match decl with
              Message_decl (_, msg) -> loop (fold_msg errs msg) arities tl
            | Type_decl (_, _, ty) -> loop (fold_ty errs ty) arities tl
    in loop [] SMap.empty l

  in dup_errors decls @ unbound_type_vars decls @ wrong_type_arities decls

let print_errors ch =
  List.iter
    (function
         Repeated_binding s -> fprintf ch "Type binding %S is duplicated.\n" s
       | Unbound_type_variable (where, which) ->
           fprintf ch "Type variable %S is unbound in %S.\n" which where
       | Wrong_arity (which, wrong, where, correct) ->
           fprintf ch "Type variable %S used with wrong arity (%d instead of %d) in %S.\n"
             which wrong correct where)
