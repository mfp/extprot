open Camlp4.PreCast
open Printf
open Syntax

type tag = int

type low_level =
    Vint of tag
  | Bitstring32 of tag
  | Bitstring64 of tag
  | Bytes of tag
  | LLTuple of tag * low_level list
  | LLHtuple of tag * int * low_level

type base_type_expr_simple = [
    `Bool
  | `Byte
  | `Int of bool
  | `Long_int of bool
  | `Float
  | `String
]

type base_type_expr = [
  base_type_expr_simple
  | `Tuple of base_type_expr list
  | `List of base_type_expr
  | `Array of base_type_expr
  | `App of string * base_type_expr list
]

type type_expr = [
    base_type_expr
  (* | `Record of (string * bool * base_type_expr) list *)
  | `Sum of sum_data_type list
]

and sum_data_type =
  Constant of string
  | Non_constant of string * base_type_expr list

type base_message_expr = [ `Record of (string * bool * base_type_expr) list ]

type message_expr = [ base_message_expr | `Sum of (string * base_message_expr) list ]

type declaration =
    Message of string * message_expr
  | Type of string * string list * type_expr

let declaration_name = function Message (n, _) | Type (n, _, _) -> n

let declaration_arity = function
    Message _ -> 0
  | Type (_, l, _) -> List.length l

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
    | `Sum l ->
        concat_map
          (function Constant _ -> []
             | Non_constant (_, l) ->
                 concat_map (type_free_vars known) (l :> type_expr list))
          l in

  let rec msg_free_vars known : message_expr -> string list = function
      `Record l ->
        concat_map (fun (_, _, e) -> type_free_vars known (e :> type_expr)) l
    | `Sum l ->
        concat_map (fun (_, e) -> msg_free_vars known (e :> message_expr)) l in

  match decl with
      Message (name, m) -> msg_free_vars [name] m
    | Type (name, tvars, e) -> type_free_vars (name :: tvars) e

let check_declarations decls =

  let dup_errors l =
    let rec loop errs bindings = function
        [] -> errs
      | decl :: tl ->
          let name = declaration_name decl in
            if SSet.mem name bindings then
              loop (Repeated_binding "name" :: errs) bindings tl
            else
              loop errs (SSet.add name bindings) tl
    in loop [] SSet.empty l in

  let unbound_type_vars l =
    let rec loop errs bindings = function
        [] -> errs
      | decl :: tl ->
          let name = declaration_name decl in
            (* add current decl to bindings to allow recursive type defs  *)
          let bindings = match decl with
              (* only recursive types, not messages = (sums of) records *)
              Type _ -> SSet.add name bindings
            | Message _ -> bindings in
          let errs = List.fold_right
                       (fun n l ->
                          if SSet.mem n bindings then l
                          else (Unbound_type_variable (name, n) :: l))
                       (free_type_variables decl)
                       errs
          in loop errs bindings tl
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
            | `Sum l -> List.fold_left
                          (fun s const -> match const with
                               Constant _ -> s
                             | Non_constant (_, l) -> List.fold_left fold_base_ty s l)
                          acc l

          in match decl with
              Message (_, msg) -> loop (fold_msg errs msg) arities tl
            | Type (_, _, ty) -> loop (fold_ty errs ty) arities tl
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
