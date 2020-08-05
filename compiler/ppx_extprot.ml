
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
          ptype_loc;
          ptype_manifest =
            Some { ptyp_desc = Ptyp_constr ({ txt = (Ldot _ as path); _ }, []); _ };
          _ } ->

          let path, name = match List.rev @@ flatten_longident_path ~loc:ptype_loc path with
            | name :: rev_path -> (List.rev rev_path, name)
            | [] -> Location.raise_errorf ~loc:ptype_loc "Invalid longindent path" in

          let mexpr = `Message_alias (path, name) in
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

let register_decl_and_gencode ~loc decl =
  rev_decls := decl :: !rev_decls;
  let decls = List.rev !rev_decls in
  let implem, signature =
    try
      G.generate_code
        ~global_opts:[]
        ~width:100
        (Gencode.collect_bindings decls) [PT.Decl decl]
    with exn ->
      Location.raise_errorf ~loc
        "Extprot codegen error: %s" (Printexc.to_string exn)
  in
    begin match Parse.implementation @@ Lexing.from_string implem with
      | _ :: str :: _ ->
          (* skip the first element: *)
          str
      | _ -> Location.raise_errorf ~loc "Codegen error"
    end

let expand_function ~force_message ~export ~loc ~path str =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
    match str with
      | [ { pstr_desc = Pstr_type (_, [ ty ]) } ] ->
          register_decl_and_gencode ~loc @@
          decl_of_ty ~force_message ~export ~loc ty
      | _ ->
          Location.raise_errorf ~loc "Expected a single type definition"

let include_attr =
  Attribute.declare "extprot.include"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let exclude_attr =
  Attribute.declare "extprot.exclude"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let split_lid_tuple e = match e.pexp_desc with
  | Pexp_ident { txt = (Lident s); _ } -> [s, (None, None)]
  | Pexp_tuple l ->
      List.map
        (function
          | { pexp_desc = Pexp_ident { txt = (Lident s); _ }; _ } ->
              (s, (None, None))
          | { pexp_loc; _ } ->
              Location.raise_errorf ~loc:pexp_loc
                "Expected a lowercase identifier")
        l
  | _ ->
      Location.raise_errorf ~loc:e.pexp_loc
        "Expected a tuple like   field1, field2, field3 "

let subset_decl_of_includes ~name ~orig inc =
  let fields = split_lid_tuple inc in
    PT.Message_decl
      (name, `Message_subset (orig, fields, `Include), PT.Export_YES, [])

let subset_decl_of_excludes ~name ~orig inc =
  let fields = split_lid_tuple inc in
    PT.Message_decl
      (name, `Message_subset (orig, fields, `Exclude), PT.Export_YES, [])

let expand_subset ~loc ~path str =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
    match str with
      | [ { pstr_desc =
              Pstr_type
                (_, [ { ptype_name = { txt = name; _ };
                        ptype_params = []; ptype_cstrs = [];
                        ptype_kind = Ptype_abstract;
                        ptype_loc;
                        ptype_manifest =
                          Some { ptyp_desc = Ptyp_constr ({ txt = Lident orig; _ }, _); _ }
                      } as tydecl ]);
            _
          }
        ] ->
          let includes = Attribute.get include_attr tydecl in
          let excludes = Attribute.get exclude_attr tydecl in
          let decl =
            match includes, excludes with
              | Some inc, None -> subset_decl_of_includes ~name ~orig inc
              | None, Some exc -> subset_decl_of_excludes ~name ~orig exc
              | None, None ->
                  Location.raise_errorf ~loc
                    "Need either [@@include a,b,c] or [@@include a,b,c] attribute"
              | Some _, Some _ ->
                  Location.raise_errorf ~loc
                    "Only one of [@@include a,b,c] or [@@include a,b,c] allowed"
          in
            register_decl_and_gencode ~loc decl
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

let extension3 =
  Ppxlib.Extension.declare
    "extprot.subset"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(pstr __)
    expand_subset

let rule1 = Ppxlib.Context_free.Rule.extension extension1
let rule2 = Ppxlib.Context_free.Rule.extension extension2
let rule3 = Ppxlib.Context_free.Rule.extension extension3

let () = Ppxlib.Driver.register_transformation ~rules:[rule1; rule2; rule3] "extprot"

let () = Ppxlib.Driver.standalone ()
