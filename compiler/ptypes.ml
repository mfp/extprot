
include Protocol_types
open ExtList

let declaration_name = function Message_decl (n, _, _) | Type_decl (n, _, _, _) -> n

let declaration_arity = function
    Message_decl _ -> 0
  | Type_decl (_, l, _, _) -> List.length l

module SSet = Set.Make(struct type t = string let compare = String.compare end)
module SMap = Map.Make(struct type t = string let compare = String.compare end)

let smap_find k m = try Some (SMap.find k m) with Not_found -> None

type error =
  | Repeated_binding of string
  | Unbound_type_variable of string * string
  | Wrong_arity of string * int * string * int
  | Unknown_type_option of string * string * string

let concat_map f l = List.concat (List.map f l)

let free_type_variables decl : string list =
  let rec free_vars known : base_type_expr -> string list = function
    | `Type_param n ->
        let s = string_of_type_param n in
          if List.mem s known then [] else [s]
    | `App (n, tys, _) ->
        let l = concat_map (free_vars known) tys in
          if List.mem n known then l else n :: l
    | `Ext_app (_, _, tys, _) -> concat_map (free_vars known) tys
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
          (non_constant_constructors sum) in

  let rec msg_free_vars known = function
    | `App (_, targs, _) ->
        concat_map (fun ty -> type_free_vars known (ty :> type_expr)) targs
    | `Record l ->
        concat_map (fun (_, _, e) -> type_free_vars known (e :> type_expr)) l
    | `Message_alias _ -> []
    | `Sum l ->
        concat_map (fun (_, e) -> msg_free_vars known (e :> message_expr)) l in

  match decl with
      Message_decl (name, m, _) -> msg_free_vars [name] m
    | Type_decl (name, tvars, e, _) ->
        type_free_vars (name :: List.map string_of_type_param tvars) e

let known_type_opts =
  [
    "default";
    "ocaml.pp";
    "ocaml.type";
    "ocaml.type_equals";
  ]

let unknown_type_opts decl : error list =

  let keep_unknown_type_opts decl_name l =
    List.filter_map
      (fun (opt, v) ->
         if List.mem opt known_type_opts then None
         else Some (Unknown_type_option (decl_name, opt, v)))
      l in

  let rec base_type_expr_errs decl_name : [< base_type_expr] -> error list = function
    | `App (_, l, opts)
    | `Ext_app (_, _, l, opts)
    | `Tuple (l, opts) ->
        keep_unknown_type_opts decl_name opts @
        List.concat @@ List.map (base_type_expr_errs decl_name) l

    | `Type_param _ -> []

    | `List (ty, opts)
    | `Array (ty, opts) ->
        keep_unknown_type_opts decl_name opts @
        base_type_expr_errs decl_name ty

    | `Bool opts | `Byte opts | `Int opts | `Long_int opts
    | `Float opts | `String opts ->
        keep_unknown_type_opts decl_name opts

  and type_expr_errs decl_name : [< type_expr] -> error list = function
    | `Record (r, opts) ->
        keep_unknown_type_opts decl_name opts @
        List.concat @@
        List.map (fun (_, _, ty) -> base_type_expr_errs decl_name ty) r.record_fields

    | `Sum (s, opts) ->
        keep_unknown_type_opts decl_name opts @
        List.concat @@
        List.map
          (function
            | `Constant _ -> []
            | `Non_constant (_, l) ->
                List.concat @@ List.map (base_type_expr_errs decl_name) l)
          s.constructors

    | #base_type_expr as x ->
        base_type_expr_errs decl_name x
  in

  let rec mexpr_errs decl_name : [< message_expr] -> _ = function
    | `Message_alias _ -> []

    | `Sum l ->
        List.concat @@
        List.map
          (fun (constr, mexpr) ->
             mexpr_errs (decl_name ^ "." ^ constr)
               (mexpr :> message_expr)) l

    | `App (_, l, opts) ->
        keep_unknown_type_opts decl_name opts @
        List.concat @@ List.map (base_type_expr_errs decl_name) l

    | `Record l ->
        List.concat @@ List.map (base_type_expr_errs decl_name) @@
        List.map (fun (_, _, ty) -> ty) l

  in
    match decl with
      | Type_decl (name, _, ty, opts) ->
          keep_unknown_type_opts name opts @
          type_expr_errs name ty

      | Message_decl (name, mexpr, opts) ->
          keep_unknown_type_opts name opts @
          mexpr_errs name mexpr

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
            | `Ext_app (_, _, tys, _) -> List.fold_left fold_base_ty acc tys
            | `App (s, params, _) ->
                let expected = List.length params in
                let acc = match smap_find s arities with
                    None -> acc
                  | Some n when n = expected -> acc
                  | Some n -> Wrong_arity (s, n, name, expected) :: acc
                in List.fold_left fold_base_ty acc params in

          let rec fold_msg acc : message_expr -> error list = function
              `Record l ->
                List.fold_left (fun errs (_, _, ty) -> fold_base_ty errs ty) acc l
            | `Message_alias _ -> acc
            | `App (s, params, _) ->
                let expected = List.length params in
                let acc = match smap_find s arities with
                    None -> acc
                  | Some n when n = expected -> acc
                  | Some n -> Wrong_arity (s, n, name, expected) :: acc
                in List.fold_left fold_base_ty acc params
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
                  acc (non_constant_constructors sum)

          in match decl with
              Message_decl (_, msg, _) -> loop (fold_msg errs msg) arities tl
            | Type_decl (_, _, ty, _) -> loop (fold_ty errs ty) arities tl
    in loop [] SMap.empty l

  in
    dup_errors decls @ unbound_type_vars decls @ wrong_type_arities decls @
    List.concat @@ List.map unknown_type_opts decls

let pp_errors pp =
  let pr fmt = Format.fprintf pp fmt in
    List.iter @@ function
    | Repeated_binding s -> pr "Type binding %S is duplicated.@." s
    | Unbound_type_variable (where, which) ->
        pr "Type %S is unbound in %S.@." which where
    | Wrong_arity (which, correct, where, wrong) ->
        pr "Type %S used with wrong arity (%d instead of %d) in %S.@."
          which wrong correct where
    | Unknown_type_option (decl_name, opt, _) ->
        pr "Unknown type option in %s: %s.@." decl_name opt
