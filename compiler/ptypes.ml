open Camlp4.PreCast
open Printf
open Syntax

type type_options = (string * string) list

let dump_type_options l =
  let pr_list name l =
    printf "%s options:\n" name;
    List.iter (fun (k, v) -> printf "%s.%S = %S\n" name k v) l in
  let dump_opt = function
      `Global l -> pr_list "Global" l
    | `OCaml l -> pr_list "OCaml" l
  in List.iter dump_opt l

type base_type_expr_simple = [
    `Bool of type_options
  | `Byte of type_options
  | `Int of type_options
  | `Long_int of type_options
  | `Float of type_options
  | `String of type_options
]

type 'a base_type_expr_core = [
    base_type_expr_simple
  | `Tuple of 'a list * type_options
  | `List of 'a * type_options
  | `Array of 'a * type_options
]

module Type_param : sig
  type type_param
  val type_param_of_string : string -> type_param
  val string_of_type_param : type_param -> string
  val type_param_name : type_param -> string
end =
struct
  type type_param = string
  let type_param_of_string x = x
  let string_of_type_param x = "'" ^ x
  let type_param_name x = x
end

include Type_param

type base_type_expr = [
    base_type_expr base_type_expr_core
  | `App of string * base_type_expr list * type_options
  | `Type_param of type_param
]

type type_expr = [
    base_type_expr
  | `Record of base_type_expr record_data_type * type_options
  | `Sum of base_type_expr sum_data_type * type_options
]

and 'a record_data_type = {
  record_name : string;
  record_fields : (string * bool * 'a) list
}

and 'a sum_data_type = {
  type_name : string;
  constant : string list;
  non_constant : (string * 'a list) list
}

type base_message_expr = [ `Record of (string * bool * base_type_expr) list ]

type message_expr = [
    base_message_expr
  | `App of string * base_type_expr list * type_options
  | `Sum of (string * base_message_expr) list
]

type declaration =
    Message_decl of string * message_expr * type_options
  | Type_decl of string * type_param list * type_expr * type_options

let base_type_expr e = (e :> base_type_expr)
let type_expr e = (e :> type_expr)

let declaration_name = function Message_decl (n, _, _) | Type_decl (n, _, _, _) -> n

let declaration_arity = function
    Message_decl _ -> 0
  | Type_decl (_, l, _, _) -> List.length l

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
    | `Type_param n ->
        let s = string_of_type_param n in
          if List.mem s known then [] else [s]
    | `App (n, tys, _) ->
        let l = concat_map (free_vars known) tys in
          if List.mem n known then l else n :: l
    | `Tuple (l, _) -> concat_map (free_vars known) l
    | `List (t, _) | `Array (t, _) -> free_vars known t
    | #base_type_expr_simple -> [] in

  let rec type_free_vars known : type_expr -> string list = function
      #base_type_expr as x -> free_vars known x
    | `Record (record, _) ->
        concat_map (fun (_, _, ty) -> free_vars known ty) record.record_fields
    | `Sum (sum, _) ->
        concat_map
          (fun (_, l) -> concat_map (type_free_vars known) (l :> type_expr list))
          sum.non_constant in

  let rec msg_free_vars known = function
    | `App (_, targs, _) ->
        concat_map (fun ty -> type_free_vars known (ty :> type_expr)) targs
    | `Record l ->
        concat_map (fun (_, _, e) -> type_free_vars known (e :> type_expr)) l
    | `Sum l ->
        concat_map (fun (_, e) -> msg_free_vars known (e :> message_expr)) l in

  match decl with
      Message_decl (name, m, _) -> msg_free_vars [name] m
    | Type_decl (name, tvars, e, _) ->
        type_free_vars (name :: List.map string_of_type_param tvars) e

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
              #base_type_expr_simple | `Type_param _ -> acc
            | `Tuple (l, _) -> List.fold_left fold_base_ty acc l
            | `List (t, _) | `Array (t, _) -> fold_base_ty acc t
            | `App (s, params, _) ->
                let expected = List.length params in
                  begin match smap_find s arities with
                      None -> acc
                    | Some n when n = expected -> acc
                    | Some n -> Wrong_arity (s, n, name, expected) :: acc
                  end in

          let rec fold_msg acc : message_expr -> error list = function
              `Record l ->
                List.fold_left (fun errs (_, _, ty) -> fold_base_ty errs ty) acc l
            | `App (s, params, _) ->
                let expected = List.length params in
                  begin match smap_find s arities with
                      None -> acc
                    | Some n when n = expected -> acc
                    | Some n -> Wrong_arity (s, n, name, expected) :: acc
                  end
            | `Sum l ->
                List.fold_left
                  (fun errs (_, msg) -> fold_msg errs (msg :> message_expr))
                  acc l in

          let fold_ty acc : type_expr -> error list = function
              #base_type_expr as bty -> fold_base_ty acc bty
            | `Record (r, _) ->
                List.fold_left
                  (fun errs (_, _, ty) -> fold_base_ty errs ty) acc r.record_fields
            | `Sum (sum, _) ->
                List.fold_left
                  (fun acc (_, l) -> List.fold_left fold_base_ty acc l)
                  acc sum.non_constant

          in match decl with
              Message_decl (_, msg, _) -> loop (fold_msg errs msg) arities tl
            | Type_decl (_, _, ty, _) -> loop (fold_ty errs ty) arities tl
    in loop [] SMap.empty l

  in dup_errors decls @ unbound_type_vars decls @ wrong_type_arities decls

let print_errors ch =
  List.iter
    (function
         Repeated_binding s -> fprintf ch "Type binding %S is duplicated.\n" s
       | Unbound_type_variable (where, which) ->
           fprintf ch "Type %S is unbound in %S.\n" which where
       | Wrong_arity (which, wrong, where, correct) ->
           fprintf ch "Type %S used with wrong arity (%d instead of %d) in %S.\n"
             which wrong correct where)
