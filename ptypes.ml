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

type base_type_expr = [
    `Bool
  | `Byte
  | `Int of bool
  | `Long_int of bool
  | `Float
  | `String
  | `Tuple of base_type_expr list
  | `List of base_type_expr
  | `Array of base_type_expr
  | `Alias of string
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


module SSet = Set.Make(struct type t = string let compare = String.compare end)

type error =
    Repeated_binding of string
  | Unbound_type_variable of string * string

let free_type_variables decl : string list =
  let rec free_vars known : base_type_expr -> string list = function
      `Alias s when List.mem s known -> []
    | `Alias s -> [s]
    | `App (n, tys) ->
        free_vars known (`Alias n) @ List.concat (List.map (free_vars known) tys)
    | `Tuple l -> List.concat (List.map (free_vars known) l)
    | `List t | `Array t -> free_vars known t
    | `Bool | `Byte | `Int _ | `Long_int _ | `Float | `String -> [] in

  let rec type_free_vars known : type_expr -> string list = function
      #base_type_expr as x -> free_vars known x
    | `Sum l ->
        List.concat
          (List.map
             (function Constant _ -> []
                | Non_constant (_, l) ->
                    List.concat
                      (List.map (type_free_vars known) (l :> type_expr list)))
             l) in

  let rec msg_free_vars known : message_expr -> string list = function
      `Record l ->
        List.concat
          (List.map (fun (_, _, e) -> type_free_vars known (e :> type_expr)) l)
    | `Sum l -> List.concat
                  (List.map (fun (_, e) -> msg_free_vars known (e :> message_expr)) l) in

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
          let errs = List.fold_right
                       (fun n l ->
                          if SSet.mem n bindings then l
                          else (Unbound_type_variable (name, n) :: l))
                       (free_type_variables decl)
                       errs
          in loop errs (SSet.add name bindings) tl
    in loop [] SSet.empty l in

  let wrong_type_arities l =
    (* TODO *)
    []

  in dup_errors decls @ unbound_type_vars decls @ wrong_type_arities decls



