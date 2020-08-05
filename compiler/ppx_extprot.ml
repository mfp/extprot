
(* ocamlfind ocamlopt -o ppx_extprot -package ppxlib,ppxlib.metaquot,extlib protocol_types.cmx ppx_extprot.ml -linkpkg *)

open Ppxlib

module PT = Protocol_types

let rec flatten_longident_path ~loc = function
  | Ldot (a, b) -> flatten_longident_path ~loc a @ [b]
  | Lident x -> [x]
  | Lapply _ -> Location.raise_errorf ~loc "Invalid longident path"

let rec tyexpr_of_core_type ?(ptype_params = []) ty =
  let module Ast_builder = (val Ast_builder.make ty.ptyp_loc) in
  let open Ast_builder in
    match ty.ptyp_desc with
      | Ptyp_tuple tys ->
          `Tuple (List.map (tyexpr_of_core_type ~ptype_params) tys, [])
      | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> `Bool []
      | Ptyp_constr ({ txt = Lident "byte"; _ }, []) -> `Byte []
      | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> `Int []
      | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> `Float []
      | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> `String []
      | Ptyp_constr ({ txt = Ldot (Lident "Int64", "t"); _ }, []) -> `Long_int []
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ty]) ->
          `List (tyexpr_of_core_type ~ptype_params ty, [])
      | Ptyp_constr ({ txt = Lident "array"; _ }, [ty]) ->
          `Array (tyexpr_of_core_type ~ptype_params ty, [])
      | Ptyp_constr ({ txt = Lident ("list" | "array"); _ }, ([] | _ :: _))
      | Ptyp_constr ({ txt = Lident ("bool" | "byte" | "int" | "float" | "string"); _ }, _ :: _)
      | Ptyp_constr ({ txt = Ldot (Lident "Int64", "t"); _ }, _ :: _) ->
          Location.raise_errorf ~loc:ty.ptyp_loc "Polymorphic type overrides primitive type"
      | Ptyp_constr ({ txt = Lident tyn; _ }, []) -> `App (tyn, [], [])
      | Ptyp_constr ({ txt = Lident tyn; _ }, tys) ->
          `App (tyn, List.map (tyexpr_of_core_type ~ptype_params) tys, [])
      | Ptyp_constr ({ txt = Ldot (path, name); loc; }, []) ->
          `Ext_app (flatten_longident_path ~loc path, name, [], [])
      | Ptyp_var s -> `Type_param (PT.Type_param.type_param_of_string s)
      | Ptyp_constr _ ->
          Location.raise_errorf ~loc:ty.ptyp_loc "Invalid type"
      | Ptyp_any
      | Ptyp_arrow _
      | Ptyp_object _
      | Ptyp_class _
      | Ptyp_alias _
      | Ptyp_variant _
      | Ptyp_poly _
      | Ptyp_package _
      | Ptyp_extension _ ->
          Location.raise_errorf ~loc:ty.ptyp_loc "Invalid type"

let field_of_label_decl pld =
  let module Ast_builder = (val Ast_builder.make pld.pld_loc) in
  let open Ast_builder in
    (pld.pld_name.txt,
     (match pld.pld_mutable with Immutable -> false | Mutable -> true),
     `Eager, tyexpr_of_core_type pld.pld_type)

let extract_type_param (ty, _) =
  match ty.ptyp_desc with
    | Ptyp_var s -> PT.Type_param.type_param_of_string s
    | _ -> Location.raise_errorf ~loc:ty.ptyp_loc "Type param is not a type var."

let rev_decls = ref []

let is_record_type name =
  List.exists
    (function
      | PT.Message_decl (n, _, _, _) -> n = name
      | PT.Type_decl (n, _, `Record _, _) -> n = name
      | PT.Type_decl _ -> false )
    !rev_decls

(* force_message: set when using  extprot.message  to (1) reject
 * anything that is not a monomorphic record type and (2) register the
 * declaration as a message declaration whose (de)serialization functions
 * are exported *)
let decl_of_ty ~export ~force_message ~loc tydecl =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
    match tydecl with
      | { ptype_name = { txt = name; _ };
          ptype_params = []; ptype_cstrs = [];
          ptype_kind = Ptype_record fs; _ } ->
          let mexpr = `Message_record (List.map field_of_label_decl fs) in
            PT.Message_decl (name, mexpr, (if export then Export_YES else Export_NO), [])

      | { ptype_name = { txt = name; _ };
          ptype_params = []; ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_manifest = Some cty; ptype_loc; _ } -> begin
          match tyexpr_of_core_type cty with
            | `App (tyn, tys, opts) when is_record_type tyn ->
                let mexpr = `Message_app (tyn, tys, opts) in
                  PT.Message_decl (name, mexpr, Export_YES, [])
            | _ when not force_message ->
                PT.Type_decl (name, [], (tyexpr_of_core_type cty :> PT.type_expr), [])
            | _ ->
                Location.raise_errorf ~loc:ptype_loc
                  "Message declaration expected (disjoin union or record type)"
        end

      | { ptype_name = { txt = name; _ };
          ptype_params; ptype_cstrs = [];
          ptype_kind = Ptype_variant constrs;
          ptype_loc;
          _ } -> begin

          (* check whether all constructors are records or tuples with
           * 1 type argument that is actually a record type application
           * *)
          match
            List.for_all
              (fun cd ->
                 match cd.pcd_args with
                   | Pcstr_record _ -> true
                   | Pcstr_tuple
                       [{ptyp_desc = Ptyp_constr ({txt = Lident name; _ }, _)}] ->
                       is_record_type name
                   | Pcstr_tuple _ -> false)
              constrs
          with
            | true when ptype_params = [] ->
                (* all constructors correspond to a record,
                 * treat this as a disjoint union of messages *)
                let branches =
                  List.map
                    (fun cd ->
                       match cd.pcd_args with
                         | Pcstr_record fs ->
                             (cd.pcd_name.txt,
                              `Message_record (List.map field_of_label_decl fs))
                         | Pcstr_tuple
                             [{ptyp_desc = Ptyp_constr ({txt = Lident name; _ }, params)}] -> begin
                             match
                               List.find_map
                                 (function
                                   | PT.Type_decl (n, _, `Record (r, _opts), _) ->
                                       if n = name then Some r.PT.record_name
                                       else None
                                   | PT.Message_decl (n, _, _, _) ->
                                       if n = name then Some n
                                       else None
                                   | _ -> None)
                                 !rev_decls
                             with
                               | None ->
                                   Location.raise_errorf ~loc:cd.pcd_loc
                                     "Unknown record type"
                               | Some recname ->
                                   (cd.pcd_name.txt,
                                    `Message_app
                                      (recname,
                                       List.map tyexpr_of_core_type params, []))
                           end
                         | Pcstr_tuple _ ->
                             Location.raise_errorf ~loc:cd.pcd_loc
                               "Invalid type in message union branch")
                    constrs in
                let mexpr = `Message_sum branches in
                  PT.Message_decl (name, mexpr, Export_YES, [])

            | _ when not force_message ->
                (* some constructor does not correspond to a record,
                 * treat this as a regular sum type *)
                let constructors =
                  List.map
                    (fun cd ->
                       match cd.pcd_args with
                         | Pcstr_record _ ->
                             Location.raise_errorf ~loc:cd.pcd_loc
                               "Unsupported inline record"
                         | Pcstr_tuple [] -> `Constant cd.pcd_name.txt
                         | Pcstr_tuple tys ->
                             `Non_constant
                               (cd.pcd_name.txt,
                                List.map (tyexpr_of_core_type ~ptype_params) tys))
                    constrs
                in
                  PT.Type_decl
                    (name, List.map extract_type_param ptype_params,
                     `Sum ({ PT.type_name = name; constructors }, []), [])

            | _ (* force_message *) ->
                Location.raise_errorf ~loc:ptype_loc
                  "Message declaration expected (disjoin union or record type)"
        end

      | _ when force_message ->
          Location.raise_errorf ~loc:tydecl.ptype_loc
            "Message declaration expected (disjoin union or record type)"

      | { ptype_name = { txt = name; _ };
          ptype_params; ptype_cstrs = [];
          ptype_kind = Ptype_record fs; _ } ->
          let r =
            { PT.record_name = name;
              record_fields = List.map field_of_label_decl fs;
            }
          in
            PT.Type_decl
              (name, List.map extract_type_param ptype_params, `Record (r, []), [])

      | { ptype_name = { txt = name; _ };
          ptype_params; ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_manifest = Some cty; _ } ->
          PT.Type_decl
            (name, List.map extract_type_param ptype_params,
             (tyexpr_of_core_type ~ptype_params cty :> PT.type_expr), [])

      | _ ->
          Location.raise_errorf ~loc "Expected a record or simple type definition"

module G = Gencode.Make(Gen_OCaml)

let expand_function ~force_message ~export ~loc ~path str =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
    match str with
      | [ { pstr_desc = Pstr_type (_, [ ty ]) } ] ->
          let decl = decl_of_ty ~force_message ~export ~loc ty in
            rev_decls := decl :: !rev_decls;
            let decls = List.rev !rev_decls in
            let implem, signature =
              G.generate_code
                ~global_opts:[]
                ~width:100
                (Gencode.collect_bindings decls) [PT.Decl decl]
            in
              begin match Parse.implementation @@ Lexing.from_string implem with
                | _ :: str :: _ ->
                    (* skip the first element: *)
                    str
                | _ -> Location.raise_errorf ~loc "Codegen error"
              end
      | _ ->
          Location.raise_errorf ~loc "Expected a single type definition"

let extension1 =
  Ppxlib.Extension.declare
    "extprot"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(pstr __)
    (expand_function ~force_message:false ~export:false)

let extension2 =
  Ppxlib.Extension.declare
    "extprot.message"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(pstr __)
    (expand_function ~force_message:true ~export:true)

let rule1 = Ppxlib.Context_free.Rule.extension extension1
let rule2 = Ppxlib.Context_free.Rule.extension extension2

let () = Ppxlib.Driver.register_transformation ~rules:[rule1; rule2] "extprot"

let () = Ppxlib.Driver.standalone ()
