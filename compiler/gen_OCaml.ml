
open Printf
open Ptypes
open Gencode
open Camlp4.PreCast
open ExtList
open ExtString

type container = {
  c_name : string;
  (* either open (in message definitions) or include (in type defs), needed for
     application of record types) *)
  c_import_modules : Ast.str_item option;
  c_types : Ast.str_item option;
  c_reader : Ast.str_item option;
  c_io_reader : Ast.str_item option;
  c_pretty_printer : Ast.str_item option;
  c_writer : Ast.str_item option;
  c_default_func : Ast.str_item option;
}

type toplevel = Ast.str_item

type entry = Toplevel of toplevel | Container of container

let empty_container name ?default_func ty_str_item =
  {
    c_name = name;
    c_import_modules = None;
    c_types = Some ty_str_item;
    c_reader = None;
    c_io_reader = None;
    c_pretty_printer = None;
    c_writer = None;
    c_default_func = default_func;
  }

let (|>) x f = f x
let (@@) f x = f x

let foldl1 msg f g = function
    [] -> invalid_arg ("foldl1: empty list -- " ^ msg)
  | hd::tl -> List.fold_left g (f hd) tl

let foldr1 msg f g = function
    [] -> invalid_arg ("foldr1: empty list -- " ^ msg)
  | l -> match List.rev l with
        [] -> assert false
      | hd::tl -> List.fold_left (fun s x -> g x s) (f hd) tl

let paLids_of_strings _loc = List.map (fun s -> <:patt< $lid:s$ >>)

let exLids_of_strings _loc = List.map (fun s -> <:expr< $lid:s$ >>)

let paCom_of_lidlist _loc l = Ast.paCom_of_list @@ paLids_of_strings _loc l

let new_lid =
  let n = ref 0 in fun base ->
    incr n;
    base ^ "_" ^ string_of_int !n

let exTup_of_lidlist _loc l = match exLids_of_strings _loc l with
    [] -> invalid_arg "exTup_of_lidlist"
  | [e] -> e
  | hd :: tl -> <:expr< ( $hd$, $Ast.exCom_of_list tl$ ) >>

let patt_of_ll_type t =
  let _loc = Loc.mk "<genererated code @ patt_of_ll_type>" in
    <:patt< Extprot.Codec.$uid:Codec.string_of_low_level_type t$ >>

let ident_with_path _loc path ident =
  List.fold_right
    (fun p id -> <:ident< $uid:p$.$id$ >>)
    path
    <:ident< $lid:ident$ >>

let maybe_all f = function
    [] -> None
  | hd :: tl ->
      let init = Option.map (fun x -> [x]) (f hd) in
      let l = List.fold_left (fun s x -> match s with
                                  None -> None
                                | Some l -> match f x with
                                      None -> None
                                    | Some y -> Some (y :: l))
                init tl
      in Option.map List.rev l

let lookup_option name ?(global = false) (opts : type_options) =
  let pick n = (global && n = name || n = "ocaml." ^ name) in
    List.fold_left (fun _ x -> Some x) None @@
    List.filter_map (function (n, v) when pick n -> Some v | _ -> None) @@
    opts

let bad_option ?msg name v = match msg with
    Some m ->
      Printf.ksprintf failwith "Bad OCaml option value for %S: %S --- %s" name v m
  | None ->
      Printf.ksprintf failwith "Bad OCaml option value for %S: %S" name v

module Caml = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Syntax))

let parse_string ?(verbose=true) kind entry s =
  try
    Gram.parse_string entry (Loc.mk "<string>") s
  with Loc.Exc_located (_, b) as e ->
    if verbose then
      Printf.eprintf "Parse error in OCaml %s: %s\nin\n%s\n"
        kind (Printexc.to_string b) s;
    raise e

let ctyp_of_path ?verbose s = parse_string ?verbose "type" Syntax.ctyp s
let expr_of_string ?verbose s = parse_string ?verbose "expression" Syntax.expr s

type type_info = { ty : string; ctyp : Ast.ctyp; fromf : Ast.expr; tof : Ast.expr; default : Ast.expr option; }

let hd_opt = function [] -> None | x :: _ -> Some x

let get_type_info opts = match lookup_option "type" opts with
  | None -> None
  | Some v ->
      let parses_ok ty fromf tof default =
        try
          ignore (ctyp_of_path ~verbose:false ty);
          ignore (expr_of_string ~verbose:false fromf);
          ignore (expr_of_string ~verbose:false tof);
          Option.may (fun s -> ignore (expr_of_string ~verbose:false s)) (hd_opt default);
          true
        with _ -> false
      in

      let split_with_semicolons () =
        match List.map String.strip @@ String.nsplit v ";" with
          | [ty; fromf; tof] -> ty, fromf, tof, None
          | [ty; fromf; tof; default] -> ty, fromf, tof, Some default
          | _ -> bad_option "type" v in

      let (ty,fromf,tof,default) =
        match List.map String.strip @@ String.nsplit v "," with
          | ty :: fromf :: tof :: ([] | [_] as default) ->
              if parses_ok ty fromf tof default then (ty, fromf, tof, hd_opt default)
              else split_with_semicolons ()
          | _ -> split_with_semicolons ()
      in
        try
          Some {
            ty = ty;
            ctyp = ctyp_of_path ty;
            fromf = expr_of_string fromf;
            tof = expr_of_string tof;
            default = Option.map expr_of_string default;
          }
        with exn -> bad_option ~msg:(Printexc.to_string exn) "type" v

let get_type default opts =
  Option.map_default (fun { ctyp; _ } -> ctyp) default (get_type_info opts)

let type_opts = function
| Vint (_, opts)
| Bitstring32 opts
| Bitstring64 (_,opts)
| Bytes opts
| Sum (_, opts)
| Record (_, _, opts)
| Htuple (_, _, opts)
| Message (_, _, opts)
| Tuple (_, opts) -> opts

let rec default_value t =
  let _loc = Loc.ghost in
  let default_value =
    match t with
    | Vint (Bool, _) -> Some <:expr< False >>
    | Vint ((Int | Int8), _) | Bitstring32 _ | Bitstring64 _ | Bytes _ -> None
    | Sum (l, _) -> begin (* first constant constructor = default*)
        match
          try
            Some (List.find_map (function `Constant x -> Some x | _ -> None) l)
          with Not_found -> None
        with
          | None -> None
          | Some c ->
              Some <:expr< $uid:String.capitalize c.const_type$.$lid:c.const_name$ >>
      end
    | Record (_name, _, _) -> None
    | Htuple (List, _, _) -> Some <:expr< [] >>
    | Htuple (Array, _, _) -> Some <:expr< [| |] >>
    | Message (path, name, _) ->
        let full_path = path @ [String.capitalize name] in
        let id = ident_with_path _loc full_path (name ^ "_default") in
        let e1 = <:expr< ! $id:id$ >> in
          Some <:expr< $e1$ () >>
    | Tuple (tys, _) -> match maybe_all default_value tys with
          None -> None
        | Some [] -> failwith "default_value: empty tuple"
        | Some [_] -> failwith "default_value: tuple with only 1 element"
        | Some (hd::tl) -> Some <:expr< ($hd$, $Ast.exCom_of_list tl$) >>
  in
  match get_type_info @@ type_opts t, default_value with
  | Some { default=None; ty; _ }, Some _ -> failwith @@ sprintf "no default value specified for external type %s" ty
  | Some { default=None; _ }, None -> None
  | Some { default=Some override; _ }, _ -> Some override
  | None, v -> v

let ident_of_ctyp ty =
  let _loc = Loc.ghost in
    try
      <:ctyp< $id:Ast.ident_of_ctyp ty$ >>
    with Invalid_argument _ -> ty

let generate_include file =
  let _loc = Loc.mk "gen_OCaml" in
  let modul = String.capitalize @@ Filename.chop_extension file in
  <:str_item< open $uid:modul$ >>

let generate_container bindings =
  let _loc = Loc.mk "gen_OCaml" in

  let typedecl name ?(params = []) ctyp =
    Ast.TyDcl (_loc, name, params, ctyp, []) in

  let typedef name ~opts ?(params = []) ctyp =
    let ctyp = get_type ctyp opts in
    <:str_item< type $typedecl name ~params ctyp $ >> in

  let type_equals ~params ty tyname =
    let applied =
      ident_of_ctyp @@
      List.fold_left
        (fun ty tyvar -> <:ctyp< $ty$ $tyvar$ >>)
        (ctyp_of_path tyname) params
    in ident_of_ctyp <:ctyp< $applied$ == $ident_of_ctyp ty$ >> in

  let maybe_type_equals opts ~params ty =
    match lookup_option "type_equals" opts with
        None -> ty
      | Some tyname  ->
          try
            type_equals ~params ty (String.strip tyname)
          with exn -> bad_option ~msg:(Printexc.to_string exn) "type_equals" tyname in

  let message_typedefs ~opts name ctyp =
    let internal = typedef ~opts:[] ("_" ^ name) ctyp in
    let ext      = typedef ~opts name <:ctyp< $ctyp$ >> in
      <:str_item< $internal$; $ext$ >> in

  let rec message_types ?(opts = []) msgname = function
    | `Message_record l ->
        let ctyp (name, mutabl, texpr) =
          let ty = ctyp_of_texpr bindings texpr in match mutabl with
              true -> <:ctyp< $lid:name$ : mutable $ty$ >>
            | false -> <:ctyp< $lid:name$ : $ty$ >> in
        let fields =
          foldl1 "message_types `Message_record" ctyp
            (fun ct field -> <:ctyp< $ct$; $ctyp field$ >>) l
        (* no quotations for type, wtf? *)
        (* in <:str_item< type $msgname$ = { $fields$ } >> *)
        in
          message_typedefs ~opts msgname <:ctyp< { $fields$ } >>

   | `Message_app (name, args, opts) ->
       let tyname = String.capitalize name ^ "." ^ String.uncapitalize name in
       let applied =
         ident_of_ctyp @@
         List.fold_left
           (fun ty tyvar -> <:ctyp< $ty$ $tyvar$ >>)
           (ctyp_of_path tyname) (List.map (ctyp_of_texpr bindings) args)
       in message_typedefs ~opts msgname <:ctyp< $applied$ >>

   | `Message_alias (path, name) ->
       let full_path = path @ [String.capitalize name; name ] in
       let uid       = String.concat "." full_path in
        message_typedefs ~opts msgname (ctyp_of_path uid)

   | `Message_sum l ->
       let tydef_of_msg_branch (const, mexpr) =
         <:str_item<
           module $String.capitalize const$ = struct
             $message_types (String.lowercase const) (mexpr :> message_expr)$
           end; >> in
       let record_types =
         foldl1 "message_types `Sum" tydef_of_msg_branch
           (fun s b -> <:str_item< $s$; $tydef_of_msg_branch b$ >>) l in

       let variant (const, _) =
         <:ctyp< $uid:const$ of ($uid:const$.$lid:String.lowercase const$) >> in
       let consts = foldl1 "message_types `Message_sum" variant
                      (fun vars c -> <:ctyp< $vars$ | $variant c$ >>) l

       in
         <:str_item<
           $record_types$;
           $message_typedefs ~opts msgname <:ctyp< [$consts$] >>$
         >>

   | `Message_subset (name, selection, sign) ->

       let bindings, l =
         match smap_find name bindings with
           | Some (Message_decl (_, `Message_record l, _)) ->
               (bindings, l)

           | Some (Message_decl (_, `Message_app (name, args, _), _)) -> begin
               match beta_reduced_msg_app_fields bindings name args with
                 | None ->
                     exit_with_error
                       "wrong message subset %s: %s is not a simple message" msgname name

                 | Some (bindings, l) -> (bindings, l)
             end

           | None | Some _ ->
               exit_with_error
                 "wrong message subset: %s is not a simple message" name in

       let subset = subset_of_selection selection sign in

       (* check that subset is not empty *)
       let () =
         match List.filter (must_keep_field subset) l with
           | _ :: _ -> ()
           | [] -> exit_with_error "Message subset %s is empty." msgname
       in

       (* check that all fields referenced in subset exist *)
       let () =
         let known = List.map (fun (name, _, _) -> name) l in
           List.iter
             (fun field ->
                if not @@ List.mem field known then
                  exit_with_error
                    "Unknown field %s referenced in message subset %s."
                    field msgname)
             selection
       in

       let ctyp (name, mutabl, texpr) =
         let ty = ctyp_of_texpr bindings texpr in match mutabl with
             true -> <:ctyp< $lid:name$ : mutable $ty$ >>
           | false -> <:ctyp< $lid:name$ : $ty$ >> in

       let fields =
         foldl1 "message_types `Message_subset" ctyp
           (fun ct ((name, _, _) as field) -> <:ctyp< $ct$; $ctyp field$ >>) @@
         List.filter (must_keep_field subset) l
       in
         message_typedefs ~opts msgname <:ctyp< { $fields$ } >>

  and modules_to_include_of_texpr = function
     | `App (name, _, _) -> Some <:str_item< include $uid:String.capitalize name$ >>
     | #type_expr -> None

  and ctyp_of_texpr bindings expr =
    reduce_to_poly_texpr_core bindings expr |> ctyp_of_poly_texpr_core bindings

  and ctyp_of_poly_texpr_core bindings = function
      `Bool opts -> get_type <:ctyp< bool >> opts
    | `Byte opts -> get_type <:ctyp< int >> opts
    | `Int opts -> get_type <:ctyp< int >> opts
    | `Long_int opts -> get_type <:ctyp< Int64.t >> opts
    | `Float opts -> get_type <:ctyp< float >> opts
    | `String opts -> get_type <:ctyp< string >> opts
    | `List (ty, opts) -> get_type <:ctyp< list ($ctyp_of_poly_texpr_core bindings ty$) >> opts
    | `Array (ty, opts) -> get_type <:ctyp< array ($ctyp_of_poly_texpr_core bindings ty$) >> opts
    | `Tuple (l, opts) -> begin match l with
          [] -> failwith "ctyp_of_poly_texpr_core: empty tuple"
        | [_] -> failwith "ctyp_of_poly_texpr_core: 1-element tuple"
        | [a; b] ->
            get_type
              <:ctyp< ( $ ctyp_of_poly_texpr_core bindings a$ * $ctyp_of_poly_texpr_core bindings b$ ) >>
              opts
        | hd::tl ->
            let tl' =
              foldr1 "ctyp_of_poly_texpr_core `Tuple" (ctyp_of_poly_texpr_core bindings)
                (fun ptexpr tup -> <:ctyp< $ ctyp_of_poly_texpr_core bindings ptexpr $ * $tup$ >>)
                tl
            in get_type
                 <:ctyp< ( $ ctyp_of_poly_texpr_core bindings hd $ * $tl'$ ) >>
                 opts
      end
    | `Ext_type (path, name, args, opts) ->
        let full_path = path @ [String.capitalize name] in
        let id = ident_with_path _loc full_path name in
        let t = List.fold_left (* apply *)
                  (fun ty ptexpr -> <:ctyp< $ty$ $ctyp_of_poly_texpr_core bindings ptexpr$ >>)
                  <:ctyp< $id:id$ >>
                  args
        in get_type
             (try <:ctyp< $id:Ast.ident_of_ctyp t$ >> with Invalid_argument _ -> t)
             opts
    | `Type (name, params, args, opts) ->
        let args = List.map (ctyp_of_poly_texpr_core bindings) args in
        let t = List.fold_left (* apply *)
                  (fun ty ty_arg -> <:ctyp< $ty$ $ty_arg$ >>)
                  <:ctyp< $uid:String.capitalize name$.$lid:name$ >>
                  args
        in
        begin match get_type_info opts with
        | None ->
          (try <:ctyp< $id:Ast.ident_of_ctyp t$ >> with Invalid_argument _ -> t)
        | Some { ctyp = t; _ } ->
          (* Substitute free variables in "ocaml.type" annotation
             with specific [args] from type application *)
          assert (List.length args = List.length params); (* checked in Ptypes *)
          let params_map = List.combine (List.map type_param_name params) args in
          let substitute = function
          | <:ctyp@_loc< '$param$ >> ->
            begin try
              <:ctyp< $List.assoc param params_map$ >>
            with
            | Not_found -> failwith @@ sprintf "Unbound type parameter '%s in %s" param name
            end
          | t -> t
          in
          (Ast.map_ctyp substitute)#ctyp t
        end
    | `Type_arg n -> <:ctyp< '$n$ >>

  in function
      Message_decl (msgname, mexpr, opts) -> begin
        let default_record ?namespace fields =
          let default_values =
            maybe_all
              (fun (name, _, llty) ->
                 Option.map (fun v -> (name, v)) (default_value llty))
              fields
          in match default_values with
              None -> <:expr< Extprot.Error.missing_field ~message:$str:msgname$ () >>
            | Some l ->
                let assigns =
                  List.map
                    (fun (name, v) -> match namespace with
                         None -> <:rec_binding< $lid:name$ = $v$ >>
                       | Some ns ->
                           <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $v$ >>)
                    l
                in <:expr< { $Ast.rbSem_of_list assigns$ } >> in

        let wrap x =
          match get_type_info opts with
              None -> x
            | Some { fromf; _ } ->
                <:expr< $fromf$ $x$ >> in

        let default_func =
          match Gencode.low_level_msg_def bindings msgname mexpr with

          | Message_single (namespace, fields) ->
              <:str_item<
                value $lid:msgname ^ "_default"$ : ref (unit -> $lid:msgname$) =
                  ref (fun () -> $ wrap (default_record ?namespace fields) $)
              >>

          | Message_alias (path, name) ->
              let full_path = path @ [String.capitalize name] in
              let v = <:expr< $id:ident_with_path _loc full_path (name ^ "_default") $ >> in
              let v = <:expr< $v$.val () >> in
                <:str_item<
                  value $lid:msgname ^ "_default"$ = ref (fun () -> $wrap v$)
                >>

          | Message_sum ((namespace, constr, fields) :: _) ->
              let namespace = Option.default constr namespace in
              let v =
                <:expr< $uid:String.capitalize constr$
                           $ default_record ~namespace fields $ >>
              in
                <:str_item<
                  value $lid:msgname ^ "_default"$ : ref (unit -> $lid:msgname$) =
                    ref (fun () -> $wrap v$)
                >>
          | Message_sum [] -> failwith "bug in generate_container: empty Message_sum list"

          | Message_subset (_, l, subset) ->
            let l = List.filter (must_keep_field subset) l in
              <:str_item<
                value $lid:msgname ^ "_default"$ : ref (unit -> $lid:msgname$) =
                  ref (fun () -> $ wrap (default_record l) $)
              >> in

        let container =
          empty_container msgname ~default_func (message_types ~opts msgname mexpr)
        in Some container
      end
    | Type_decl (name, params, texpr, opts) ->
        let ty = match poly_beta_reduce_texpr bindings texpr with
          | `Record (r, _) -> begin
              let ctyp (name, mutabl, texpr) =
                let ty = ctyp_of_poly_texpr_core bindings texpr in match mutabl with
                    true -> <:ctyp< $lid:name$ : mutable $ty$ >>
                  | false -> <:ctyp< $lid:name$ : $ty$ >> in
              let fields =
                foldl1 "ctyp_of_poly_texpr_core `Type_decl (_, _, `Record _, _)" ctyp
                  (fun ct field -> <:ctyp< $ct$; $ctyp field$ >>) r.record_fields
              in <:ctyp< { $fields$ } >>
            end
          | `Sum (s, _) -> begin
              let ty_of_const_texprs (const, ptexprs) =
                (* eprintf "type %S, const %S, %d ptexprs\n" name const (List.length ptexprs); *)
                let tys = List.map (ctyp_of_poly_texpr_core bindings) ptexprs in
                  <:ctyp< $uid:const$ of $Ast.tyAnd_of_list tys$>>

              in let sum_ty =
                foldl1 "generate_container Type_decl `Sum"
                  (function
                     | `Constant tyn -> <:ctyp< $uid:tyn$ >>
                     | `Non_constant l -> ty_of_const_texprs l)
                  (fun ctyp c -> match c with
                     | `Constant tyn -> <:ctyp< $ctyp$ | $uid:tyn$ >>
                     | `Non_constant c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$>>)
                  s.constructors
                (*
                match s.constant with
                  [] -> foldl1 "generate_container Type_decl `Sum"
                          ty_of_const_texprs
                          (fun ctyp c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$ >>)
                          s.non_constant
                | _ ->
                    let const =
                      foldl1 "generate_container `Sum"
                        (fun tyn -> <:ctyp< $uid:tyn$ >>)
                        (fun ctyp tyn -> <:ctyp< $ctyp$ | $uid:tyn$ >>)
                        s.constant

                    in List.fold_left
                         (fun ctyp c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$ >>)
                         const s.non_constant
                 *)
              in <:ctyp< [ $sum_ty$ ] >>
            end
          | #poly_type_expr_core as ptexpr ->
              get_type (ctyp_of_poly_texpr_core bindings ptexpr) opts in
        let params =
          List.map (fun n -> <:ctyp< '$lid:type_param_name n$ >>) params in
        let type_rhs = maybe_type_equals opts ~params ty in
        let container = empty_container name (typedef name ~params ~opts type_rhs) in
          Some { container with c_import_modules = modules_to_include_of_texpr texpr }

let loc = Camlp4.PreCast.Loc.mk

let maybe_str_item =
  let _loc = loc "<generated code>" in
    Option.default <:str_item< >>

module PrOCaml =Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax)

let string_of_ast ?width f ast =
  let b = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer b in
  let o = new PrOCaml.printer () in
    Option.may (Format.pp_set_margin fmt) width;
    Format.fprintf fmt "@[<v0>%a@]@." (f o) ast;
    Buffer.contents b

let default_function = function
    None -> None
  | Some _ -> assert false

let generate_code ?width containers =
  let _loc = loc "<generated code>" in
  let container_of_str_item c =
    <:str_item<
       module $String.capitalize c.c_name$ = struct
         $maybe_str_item c.c_import_modules$;
         $maybe_str_item c.c_types$;
         $maybe_str_item c.c_default_func$;
         $maybe_str_item c.c_pretty_printer$;
         $maybe_str_item c.c_reader$;
         $maybe_str_item c.c_io_reader$;
         $maybe_str_item c.c_writer$
       end >>
  in string_of_ast ?width (fun o -> o#implem)
       (List.fold_left
          begin fun s e ->
            match e with
            | Toplevel t -> <:str_item< $s$; $t$; >>
            | Container c -> <:str_item< $s$; $container_of_str_item c$ >>
          end
          <:str_item< >>
          containers)

let wrap_value opts expr =
  let _loc = Loc.ghost in
  match get_type_info opts with
  | Some { tof; _ } -> <:expr< $tof$ $expr$ >>
  | None -> expr

module Pretty_print =
struct
  let _loc = Loc.mk "Gen_OCaml.Pretty_print"

  let expr_of_list l =
    List.fold_right (fun x l -> <:expr< [ $x$ :: $l$ ] >>) l <:expr< [] >>

  let pp_func name = <:expr< Extprot.Pretty_print.$lid:name$ >>

  let pp_name path name = <:expr< $id:ident_with_path _loc path ("pp_" ^ name)$ >>

  let rec pp_message ?namespace bindings msgname = function
    | `Message_record l -> pp_message_record ?namespace bindings msgname l

    | (`Message_subset (name, selection, sign) : message_expr) as _mexpr ->
        let bindings, l =
          match smap_find name bindings with
            | Some (Message_decl (_, `Message_record l, _)) ->
                  (bindings, l)

            | Some (Message_decl (_, `Message_app (name, args, _), _)) -> begin
                match beta_reduced_msg_app_fields bindings name args with
                  | None ->
                      exit_with_error
                        "wrong message subset %s: %s is not a simple message" msgname name

                  | Some (bindings, l) -> (bindings, l)
              end

            | None | Some _ ->
                exit_with_error
                  "wrong message subset %s: %s is not a simple message" msgname name
        in
          pp_message_record bindings msgname @@
          List.filter (must_keep_field @@ subset_of_selection selection sign) l

    | `Message_app(name, args, _) ->
        let pp_func =
          List.fold_left
            (fun e ptexpr -> <:expr< $e$ $pp_texpr bindings ptexpr$ >>)
            (pp_name [String.capitalize name] name)
            args
        in <:expr< $pp_func$ pp >>
    | `Message_alias (path, name) ->
        let full_path = path @ [String.capitalize name] in
        <:expr< $pp_name full_path name$ pp >>
    | `Message_sum l ->
        let match_case (const, mexpr) =
          <:match_case<
            $uid:const$ t ->
              $pp_message ~namespace:const bindings msgname (mexpr :> message_expr)$ t>>
        in <:expr< fun [ $Ast.mcOr_of_list (List.map match_case l)$ ] >>

  and pp_message_record ?namespace bindings msgname l =
    let pp_field i (name, _, tyexpr) =
      let selector = match namespace with
          None -> <:expr< (fun t -> t.$lid:name$) >>
        | Some ns -> <:expr< (fun t -> t.$uid:String.capitalize ns$.$lid:name$) >> in
      let prefix = match namespace with
          None -> String.capitalize msgname
        | Some ns -> sprintf "%s.%s"
                       (String.capitalize msgname) (String.capitalize ns) in
      let label =
        if i = 0 then prefix ^ "." ^ name
        else name
      in <:expr<
               ( $str:label$,
                 $pp_func "pp_field"$ $selector$ $pp_texpr bindings tyexpr$ )
             >> in
    let pp_fields = List.mapi pp_field l in
      <:expr< $pp_func "pp_struct"$ $expr_of_list pp_fields$ pp >>

  and pp_texpr bindings texpr =
    reduce_to_poly_texpr_core bindings texpr |> pp_poly_texpr_core bindings

  and pp_poly_type bindings path name args =
    let path = path @ [String.capitalize name] in
    List.fold_left
      (fun e ptexpr -> <:expr< $e$ $pp_poly_texpr_core bindings ptexpr$ >>)
      (pp_name path name)
      args

  and pp_poly_texpr_core bindings = function
      `Bool _ -> pp_func "pp_bool"
    | `Byte _ -> pp_func "pp_int"
    | `Int _ -> pp_func "pp_int"
    | `Long_int _ -> pp_func "pp_int64"
    | `Float _ -> pp_func "pp_float"
    | `String _ -> pp_func "pp_string"
    | `List (ty, _) -> <:expr< $pp_func "pp_list"$ $pp_poly_texpr_core bindings ty$ >>
    | `Array (ty, _) -> <:expr< $pp_func "pp_array"$ $pp_poly_texpr_core bindings ty$ >>
    | `Tuple ([ty], _) -> pp_poly_texpr_core bindings ty
    | `Tuple (l, _) ->
        List.fold_left
          (fun e ptexpr -> <:expr< $e$ $pp_poly_texpr_core bindings ptexpr$ >>)
          (pp_func ("pp_tuple" ^ string_of_int (List.length l)))
          l
    | `Type (name, _params, args, _) -> pp_poly_type bindings [] name args
    | `Ext_type (path, name, args, _) -> pp_poly_type bindings path name args
    | `Type_arg n -> <:expr< $lid:"pp_" ^ n$ >>

  let add_msgdecl_pretty_printer bindings msgname mexpr opts c =
    let expr = pp_message bindings msgname mexpr in
    { c with c_pretty_printer =
        Some
          <:str_item<
            value $lid:"pp_" ^ msgname$ pp x = $expr$ ($wrap_value opts <:expr<x>>$);
            value pp = $lid:"pp_" ^ msgname$; >> }

  let add_typedecl_pretty_printer bindings tyname typarams texpr opts c =
    let wrap expr =
      List.fold_right
        (fun typaram e ->
           <:expr< fun $lid:"pp_" ^ type_param_name typaram$ -> $e$ >>)
        typarams
        expr in

    let expr = match poly_beta_reduce_texpr bindings texpr with
      | `Sum (s, opts) -> begin
          let constr_ptexprs_case (const, ptexprs) =
            let params = Array.to_list @@
                         Array.init (List.length ptexprs) (sprintf "v%d")
            in
              <:match_case<
                $uid:const$ $paCom_of_lidlist _loc params$ ->
                  $pp_func "fprintf"$ pp
                  $str:String.capitalize tyname ^ "." ^ const ^ " %a"$
                  $pp_poly_texpr_core bindings (`Tuple (ptexprs, opts))$ ($exTup_of_lidlist _loc params$)
              >> in
          let constr_case constr =
            <:match_case<
                $uid:constr$ -> $pp_func "fprintf"$ pp
                                  $str:String.capitalize tyname ^ "." ^ constr$
            >> in
          let cases = List.map
                        (function
                           | `Constant c -> constr_case c
                           | `Non_constant (n, l) -> constr_ptexprs_case (n, l))
                        s.constructors
          in
          <:expr< fun pp -> fun x -> match ($wrap_value opts <:expr<x>>$) with [ $Ast.mcOr_of_list cases$ ] >>
        end
      | `Record (r, opts) -> begin
          let pp_field i (name, _, tyexpr) =
            let field_name =
              if i = 0 then String.capitalize r.record_name ^ "." ^ name
              else name
            in
              <:expr<
                ( $str:field_name$,
                  $pp_func "pp_field"$ (fun t -> t.$lid:name$) $pp_poly_texpr_core bindings tyexpr$ )
              >> in
          let pp_fields = List.mapi pp_field r.record_fields in
          <:expr< fun ppf -> fun x -> $pp_func "pp_struct"$ $expr_of_list pp_fields$ ppf ($wrap_value opts <:expr<x>>$) >>
        end
      | #poly_type_expr_core as ptexpr ->
          let ppfunc_expr = pp_poly_texpr_core bindings ptexpr in
          <:expr< fun ppf -> fun x -> $ppfunc_expr$ ppf ($wrap_value opts <:expr<x>> $) >>
    in
      { c with c_pretty_printer =
          Some <:str_item< value $lid:"pp_" ^ tyname$ = $wrap expr$ >> }

  let add_typedecl_pretty_printer bindings tyname typarams texpr opts c =
    match lookup_option "pp" opts with
        None -> add_typedecl_pretty_printer bindings tyname typarams texpr opts c
      | Some s ->
      { c with c_pretty_printer =
          Some <:str_item< value $lid:"pp_" ^ tyname$ = $expr_of_string s$ >> }
end

let partition_constructors l =
  let c = List.filter_map (function `Constant x -> Some x | _ -> None) l in
  let nc = List.filter_map (function `Non_constant x -> Some x | _ -> None) l in
    (c, nc)

module Make_reader
         (RD : sig
            val name        : string
            val reader_func : Reader.reader_func -> Ast.expr
            val raw_rd_func : low_level -> (Ast.patt * Ast.expr) option
            val read_msg_func : string -> string
          end) =
struct
  let _loc = Loc.mk "<generated code at Make_reader>"

  let use_locs opts =
    match List.assoc "locs" opts with
      | exception Not_found -> true
      | "false" -> false
      | _ -> true

  let wrap_reader opts expr = match get_type_info opts with
      Some { fromf; _ } ->
        <:expr<
          try
            $fromf$ $expr$
          with [ Extprot.Error.Extprot_error _ as e -> raise e
               | e -> Extprot.Error.conversion_error e]
        >>
    | None -> expr

  let rec read_field msgname constr_name name llty =
    let _loc = loc "<generated code @ read_field>" in

    let rec read_elms f lltys_and_defs =
      (* TODO: handle missing elms *)
      let vars = List.rev @@ Array.to_list @@
                 Array.init (List.length lltys_and_defs) (sprintf "v%d")
      in
        List.fold_right
          (fun (n, llty, default) e ->
             let varname = sprintf "v%d" n in
               match default with
                   None ->
                     <:expr<
                       let $lid:varname$ =
                         if nelms >= $int:string_of_int (n+1)$ then
                           $read llty$
                         else
                           Extprot.Error.missing_tuple_element
                             ~message:$str:msgname$
                             ~constructor:$str:constr_name$
                             ~field:$str:name$
                             $int:string_of_int n$
                       in $e$
                     >>
                 | Some expr ->
                     <:expr<
                       let $lid:varname$ =
                         if nelms >= $int:string_of_int (n+1)$ then
                           $read llty$
                         else $expr$
                       in $e$
                     >>)
          (List.mapi (fun i (ty, default) -> (i, ty, default)) lltys_and_defs)
          (f (List.rev vars))

    and read_tuple_elms lltys_and_defs =
      let mk_tup vars =
        Ast.exCom_of_list @@ List.map (fun v -> <:expr< $lid:v$ >>) vars
      in read_elms mk_tup lltys_and_defs

    and read_sum_elms constr lltys =
      let c = constr in
      let mk_expr vars =
        List.fold_left
          (fun e var -> <:expr< $e$ $lid:var$ >>)
          <:expr< $uid:String.capitalize c.const_type$.$uid:c.const_name$ >>
          vars
      in read_elms mk_expr lltys

    and read t =
      let expr = match t with
      | Vint (Bool, _) -> <:expr< $RD.reader_func `Read_bool$ s >>
      | Vint (Int, _) -> <:expr< $RD.reader_func `Read_rel_int$ s >>
      | Vint (Int8, _) -> <:expr< $RD.reader_func `Read_i8$ s >>
      | Bitstring32 _ -> <:expr< $RD.reader_func `Read_i32$ s >>
      | Bitstring64 (Long, _) -> <:expr< $RD.reader_func `Read_i64$ s >>
      | Bitstring64 (Float, _) -> <:expr< $RD.reader_func `Read_float$ s >>
      | Bytes _ -> <:expr< $RD.reader_func `Read_string$ s >>
      | Tuple (lltys, _) ->
          let bad_type_case =
            <:match_case< ll_type -> Extprot.Error.bad_wire_type ~ll_type () >> in
          let other_cases = match lltys with
              [ty] -> begin match RD.raw_rd_func ty with
                  Some (mc, reader_expr) ->
                    <:match_case< $mc$ -> $reader_expr$ s | $bad_type_case$ >>
                | None -> bad_type_case
              end
            (* handle missing elements when expanding the primitive type to
             * a Tuple; needs default values *)
            | ty :: tys -> begin match RD.raw_rd_func ty with
                | Some (mc, reader_expr) -> begin match maybe_all default_value tys with
                      None -> bad_type_case
                    | Some defs ->
                        <:match_case<
                            $mc$ -> ($reader_expr$ s, $Ast.exCom_of_list defs$)
                          | $bad_type_case$
                        >>
                  end
                | None -> bad_type_case
              end
            | _ -> (* can't happen *) bad_type_case in
          let tys_with_defvalues =
            List.map (fun llty -> (llty, default_value llty)) lltys
          in <:expr<
               let t = $RD.reader_func `Read_prefix$ s in
                 match Extprot.Codec.ll_type t with [
                     Extprot.Codec.Tuple ->
                       let len = $RD.reader_func `Read_vint$ s in
                       let eot = $RD.reader_func `Offset$ s len in
                       let nelms = $RD.reader_func `Read_vint$ s in
                       let v = $read_tuple_elms tys_with_defvalues$ in begin
                         $RD.reader_func `Skip_to$ s eot;
                         v
                       end
                   | $other_cases$
                 ]
             >>
      | Sum (constructors, _) ->
          let constant, non_constant = partition_constructors constructors in
          let constant_match_cases =
            List.map
              (fun c ->
                 <:match_case<
                   $int:string_of_int c.const_tag$ ->
                     $uid:String.capitalize c.const_type$.$lid:c.const_name$
                 >>)
              constant
            @ [ <:match_case<
                  tag -> Extprot.Error.unknown_tag tag >> ] in

          let nonconstant_match_cases =
            let mc (c, lltys) =
              <:match_case<
                 $int:string_of_int c.const_tag$ ->
                      $read_sum_elms c (List.map (fun llty -> (llty, default_value llty)) lltys)$
              >>
            in List.map mc non_constant @
               [ <:match_case<
                   tag -> Extprot.Error.unknown_tag tag >> ]
          in

          let maybe_match_case (llty, f, l) = match l with
              [] | [_] (* catch-all *)-> None
            | l ->
                let expr =
                  <:expr< match Extprot.Codec.ll_tag t with [ $Ast.mcOr_of_list l$ ] >>
                in
                  Some <:match_case< $patt_of_ll_type llty$ -> $f expr$ >> in

          let wrap_non_constant e =
            <:expr<
              let len = $RD.reader_func `Read_vint$ s in
              let eot' = $RD.reader_func `Offset$ s len in
              let nelms = $RD.reader_func `Read_vint$ s in
              let v = $e$ in begin
                $RD.reader_func `Skip_to$ s eot';
                v
              end >> in

          let match_cases =
            List.filter_map maybe_match_case
              [
                Codec.Enum, (fun e -> e), constant_match_cases;
                Codec.Tuple, wrap_non_constant, nonconstant_match_cases;
              ] in

          let bad_type_case =
            <:match_case< ll_type -> Extprot.Error.bad_wire_type ~ll_type () >> in

          let other_cases = match non_constant with
              (c, [ty]) :: _ -> begin match RD.raw_rd_func ty with
                  Some (mc, reader_expr) ->
                    <:match_case<
                        $mc$ -> $uid:String.capitalize c.const_type$.$uid:c.const_name$ ($reader_expr$ s)
                      | $bad_type_case$
                    >>
                | None -> bad_type_case
              end
            | (c, ty :: tys) :: _ -> begin match RD.raw_rd_func ty with
                  Some (mc, reader_expr) -> begin match maybe_all default_value tys with
                      None -> bad_type_case
                    | Some defs ->
                        <:match_case<
                            $mc$ ->
                               $uid:String.capitalize c.const_type$.$uid:c.const_name$
                                 ($reader_expr$ s, $Ast.exCom_of_list defs$)
                          | $bad_type_case$
                        >>
                  end
                | None -> bad_type_case
              end
            | _ -> bad_type_case
          in
            <:expr< let t = $RD.reader_func `Read_prefix$ s in
              match Extprot.Codec.ll_type t with [
                $Ast.mcOr_of_list match_cases$
                | $other_cases$
              ]
            >>
      | Record (name, fields, opts) ->
          let fields' =
            List.map (fun f -> (f.field_name, true, f.field_type)) fields in
          let promoted_match_cases =
            make_promoted_match_cases msgname [ Some name, None, fields' ] in
          let match_cases =
            record_case_inlined
              ~locs:(use_locs opts) ~namespace:name msgname 0 fields'
          in
            wrap_msg_reader name ~promoted_match_cases match_cases

      | Message (path, name, _) ->
          let full_path = path @ [String.capitalize name] in
          let id = ident_with_path _loc full_path (RD.read_msg_func name) in
          <:expr< $id:id$ s >>
      | Htuple (kind, llty, _) ->
          let e = match kind with
              List ->
                let loop = new_lid "loop" in
                  <:expr<
                    let rec $lid:loop$ acc = fun [
                        0 -> List.rev acc
                      | n -> let v = $read llty$ in $lid:loop$ [v :: acc] (n - 1)
                    ] in $lid:loop$ [] nelms
                  >>
              | Array ->
                  <:expr< Array.init nelms (fun _ -> $read llty$) >>
          in <:expr<
                let t = $RD.reader_func `Read_prefix$ s in
                  match Extprot.Codec.ll_type t with [
                      Extprot.Codec.Htuple ->
                        let len = $RD.reader_func `Read_vint$ s in
                        let eoht = $RD.reader_func `Offset$ s len in
                        let nelms = $RD.reader_func `Read_vint$ s in
                        let () = Extprot.Limits.check_message_length len in
                        let () = Extprot.Limits.check_num_elements nelms in
                        let v = $e$ in begin
                          $RD.reader_func `Skip_to$ s eoht;
                          v
                        end
                    | ty -> Extprot.Error.bad_wire_type ~ll_type:ty ()
                  ]
              >>
    in
    wrap_reader (type_opts t) expr
    in
    read llty

  and field_reader_funcname ~msgname ~constr ~name =
    let mangle s =
      let b = Buffer.create 13 in
        String.iter
          (function
            | '_' -> Buffer.add_string b "__"
            | c -> Buffer.add_char b c)
          s;
        Buffer.contents b
    in
      sprintf "__%s_read_x%s_x%s_x%s"
        RD.name (mangle msgname) (Option.map_default mangle "" constr) (mangle name)

  and record_case_field_readers msgname ~locs ?constr _ fields =
    let _loc = Loc.mk "<generated code @ record_case_field_readers>" in
    let constr_name = Option.default "<default>" constr in

    let read_field fieldno (name, _, llty) =
      let rescue_match_case = match default_value llty with
          None ->
            <:match_case<
              Extprot.Error.Extprot_error (e, loc) ->
                Extprot.Error.failwith_location
                  ~message:$str:msgname$
                  ~constructor:$str:constr_name$
                  ~field:$str:name$
                  e loc
            >>
        | Some _expr ->
            <:match_case<
                Extprot.Error.Extprot_error
                  ((Extprot.Error.Bad_format (Extprot.Error.Bad_wire_type _) as e), loc) ->
                    Extprot.Error.failwith_location
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      e loc >> in
      let default = match default_value llty with
          Some expr -> expr
        | None ->
            <:expr< Extprot.Error.missing_field
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      ()
            >> in

      let funcname  = field_reader_funcname ~msgname ~constr ~name in

      let read_expr =
        if locs then
          <:expr<
              try
                $read_field msgname constr_name name llty$
              with [$rescue_match_case$]
          >>
        else
          read_field msgname constr_name name llty
      in
        <:str_item<
          value $lid:funcname$ s nelms =
             if nelms >= $int:string_of_int (fieldno + 1)$ then
               $read_expr$
             else $default$
        >>
    in
      List.fold_right
        (fun (i, fieldinfo) functions ->
           <:str_item< $read_field i fieldinfo$; $functions$ >>)
        (List.mapi (fun i x -> (i, x)) fields)
        <:str_item< >>

  and record_case_inlined msgname ~locs ?namespace ?constr tag fields =
    let _loc        = Loc.mk "<generated code @ record_case_inlined>" in
    let constr_name = Option.default "<default>" constr in

    let read_field fieldno (name, _, llty) expr =

      let rescue_match_case = match default_value llty with
          None ->
            <:match_case<
              Extprot.Error.Extprot_error (e, loc) ->
                Extprot.Error.failwith_location
                  ~message:$str:msgname$
                  ~constructor:$str:constr_name$
                  ~field:$str:name$
                  e loc
            >>
        | Some _expr ->
            <:match_case<
                Extprot.Error.Extprot_error
                  ((Extprot.Error.Bad_format (Extprot.Error.Bad_wire_type _) as e), loc) ->
                    Extprot.Error.failwith_location
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      e loc >> in
      let default = match default_value llty with
          Some expr -> expr
        | None ->
            <:expr< Extprot.Error.missing_field
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      ()
            >> in

      let read_expr =
        if locs then
          <:expr<
            try
              $read_field msgname constr_name name llty$
            with [$rescue_match_case$]
          >>
        else
          read_field msgname constr_name name llty
      in
        <:expr<
           let $lid:name$ =
             if nelms >= $int:string_of_int (fieldno + 1)$ then
               $read_expr$
             else
                 $default$
           in $expr$
        >> in

    let field_assigns =
      List.map
        (fun (name, _, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:name$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $lid:name$ >>)
        fields in
    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in
    let e =
      List.fold_right
        (fun (i, fieldinfo) e -> read_field i fieldinfo e)
        (List.mapi (fun i x -> (i, x)) fields)
        record
    in
      <:match_case<
        $int:string_of_int tag$ ->
          try
            let v = $e$ in begin
              $RD.reader_func `Skip_to$ s eom;
              v
            end
          with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
      >>

  and record_case msgname ?namespace ?constr tag fields =
    let _loc = Loc.mk "<generated code @ record_case>" in

    let read_field _ (name, _, _) expr =
      let funcname = field_reader_funcname ~msgname ~constr ~name in
        <:expr<
          let $lid:name$ = $lid:funcname$ s nelms in
            $expr$
        >>
    in

    let field_assigns =
      List.map
        (fun (name, _, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:name$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $lid:name$ >>)
        fields in
    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in
    let e =
      List.fold_right
        (fun (i, fieldinfo) e -> read_field i fieldinfo e)
        (List.mapi (fun i x -> (i, x)) fields)
        record
    in
      <:match_case<
        $int:string_of_int tag$ ->
          try
            let v = $e$ in begin
              $RD.reader_func `Skip_to$ s eom;
              v
            end
          with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
      >>

  and subset_case ~orig msgname ?namespace ?constr tag fields ~subset =
    let _loc        = Loc.mk "<generated code @ subset_case>" in
    (* let constr_name = Option.default "<default>" constr in *)

    let read_field fieldno ((name, _, llty) as field) expr =
      if not @@ must_keep_field subset field then
        <:expr<
          let () =
            if nelms >= $int:string_of_int (fieldno + 1)$ then
              ignore ($RD.reader_func `Skip_value$ s ($RD.reader_func `Read_prefix$ s))
            else ()
          in
            $expr$
        >>
      else
        let funcname = field_reader_funcname ~msgname:orig ~constr ~name in
          <:expr<
            let $lid:name$ = $uid:String.capitalize orig$.$lid:funcname$ s nelms in
              $expr$
          >> in

    let field_assigns =
      List.map
        (fun (name, _, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:name$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $lid:name$ >>) @@
      List.filter (must_keep_field subset) fields in

    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in
    let e =
      List.fold_right
        (fun (i, fieldinfo) e -> read_field i fieldinfo e)
        (List.mapi (fun i x -> (i, x)) @@
         List.fold_right
           (fun ((name, _, _) as f) fs -> match fs with
              | _ :: _ -> f :: fs
              | [] -> if must_keep_field subset f then f :: fs else [])
           fields [])
        record
    in

      (* check that the subset is not empty *)
      begin
        match List.map (must_keep_field subset) fields with
          | _ :: _ -> ()
          | [] -> exit_with_error "Message subset %s is empty." msgname
      end;

      <:match_case<
        $int:string_of_int tag$ ->
          try
            let v = $e$ in begin
              $RD.reader_func `Skip_to$ s eom;
              v
            end
          with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
      >>

  and wrap_msg_reader msgname ?promoted_match_cases match_cases =
    let _loc       = Loc.mk "<generated code @ wrap_msg_reader>" in
    let promo_expr =
      match promoted_match_cases with
        | Some promoted_match_cases ->
            <:expr<
              let raise_bad_wire_type () =
                 Extprot.Error.bad_wire_type
                   ~message:$str:msgname$ ~ll_type:(Extprot.Codec.ll_type t) ()
              in
                match Extprot.Codec.ll_tag t with [
                  $promoted_match_cases$
                | tag -> Extprot.Error.unknown_tag ~message:$str:msgname$ tag
                ]
            >>
        | None ->
            <:expr<
              Extprot.Error.bad_wire_type
                ~message:$str:msgname$ ~ll_type:(Extprot.Codec.ll_type t) ()
            >>
    in
      <:expr<
        let t = $RD.reader_func `Read_prefix$ s in begin
          if Extprot.Codec.ll_type t = Extprot.Codec.Tuple then
            let len   = $RD.reader_func `Read_vint$ s in
            let eom   = $RD.reader_func `Offset$ s len in
            let nelms = $RD.reader_func `Read_vint$ s in
            let () = Extprot.Limits.check_num_elements ~message:$str:msgname$ nelms in
            let () = Extprot.Limits.check_message_length ~message:$str:msgname$ len in
              match Extprot.Codec.ll_tag t with [
                $match_cases$
                | tag -> Extprot.Error.unknown_tag ~message:$str:msgname$ tag
              ]
          else $promo_expr$
        end
      >>

  and promoted_type_reader ?namespace ?constr msgname fields =
    let constr_name = Option.default "<default>" constr in

    let first_field_expr =
      match fields with
          [] -> failwith (sprintf "no fields in msg %s" msgname)
        | (_field_name, _, field_type) :: _ ->
            match RD.raw_rd_func field_type with
                None -> <:expr< raise_bad_wire_type () >>
              | Some (mc, reader_expr) ->
                <:expr<
                  match Extprot.Codec.ll_type t with [
                      $mc$ -> $reader_expr$ s
                    | _ -> raise_bad_wire_type ()
                  ]
                >> in

    let other_field_readers =
      match fields with
          [] | [_] -> []
        | _ :: others ->
            List.map
              (fun (name, _, llty) ->
                 match default_value llty with
                     Some expr -> expr
                   | None ->
                       <:expr< Extprot.Error.missing_field
                                 ~message:$str:msgname$
                                 ~constructor:$str:constr_name$
                                 ~field:$str:name$
                                 ()
                       >>)
              others in

    let field_assigns =
      List.mapi
        (fun i (name, _, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:sprintf "v%d" i$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ =
                                          $lid:sprintf "v%d" i$ >>)
        fields in
    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in

    let read_others_and_assign =
      List.fold_right
        (fun (idx, read_expr) e ->
           <:expr<
             let $lid:sprintf "v%d" idx$ = $read_expr$ in $e$ >>)
        (List.mapi (fun i r -> (i + 1, r)) other_field_readers)
        record
    in
      <:expr<
         let v0 = $first_field_expr$ in
           $read_others_and_assign$ >>

  and make_promoted_match_cases msgname field_infos =
    List.mapi
      (fun tag (namespace, constr, fields) ->
         <:match_case<
           $int:string_of_int tag$ ->
             $promoted_type_reader ?namespace ?constr msgname fields$ >>)
      field_infos |>
    Ast.mcOr_of_list

  and read_message ?(inline = false) msgname opts = function
      | Message_single (namespace, fields) ->

          let promoted_match_cases =
            make_promoted_match_cases msgname [ namespace, None, fields ] in

          let match_cases, field_readers =
            if inline then
              let match_cases =
                record_case_inlined ~locs:(use_locs opts) ?namespace msgname 0 fields
              in
                (match_cases, <:str_item< >>)
            else
              let locs          = use_locs opts in
              let match_cases   = record_case ?namespace msgname 0 fields in
              let field_readers = record_case_field_readers ~locs msgname 0 fields in
                (match_cases, field_readers) in

          let main_expr =
            wrap_msg_reader msgname ~promoted_match_cases match_cases |>
            wrap_reader opts
          in
            (field_readers, main_expr)

      | Message_subset (orig, fields, subset) ->
          let match_cases = subset_case ~orig msgname 0 fields ~subset in
          let main_expr = wrap_msg_reader msgname match_cases |> wrap_reader opts in
            (<:str_item< >>, main_expr)

      | Message_alias (path, name) ->
          let full_path = path @ [String.capitalize name] in
          let _loc = Loc.mk "<generated code @ read_message>" in
          let reader_func = RD.read_msg_func name in
          let main_expr =
            wrap_reader opts
            <:expr< $id:ident_with_path _loc full_path reader_func$ s >>
          in
            (<:str_item< >>, main_expr)

      | Message_sum l ->
          let promoted_match_cases =
            make_promoted_match_cases msgname
              (List.map
                 (fun (ns, constr, fields) ->
                    (Some (Option.default constr ns), Some constr, fields))
                 l) in

          let locs = use_locs opts in

          let field_readers =
            List.fold_left
              (fun funcs func ->
                 <:str_item< $func$; $funcs$ >>)
              <:str_item< >> @@
            List.rev @@
            List.mapi
              (fun tag (_namespace, constr, fields) ->
                 record_case_field_readers ~locs ~constr msgname tag fields)
              l in

          let match_cases =
            List.mapi
              (fun tag (namespace, constr, fields) ->
                 record_case
                   ~namespace:(Option.default constr namespace)
                   msgname ~constr tag fields) l
            |> Ast.mcOr_of_list in

          let main_expr =
            wrap_msg_reader msgname ~promoted_match_cases match_cases |>
            wrap_reader opts
          in
            (field_readers, main_expr)
end

let raw_rd_func reader_func =
  let _loc = Loc.ghost in
  let wrap t readerf = match get_type_info @@ type_opts t with
        Some { fromf; _ } ->
          <:expr<
            (fun s ->
               try
                 $fromf$ ($readerf$ s)
               with [ Extprot.Error.Extprot_error _ as e -> raise e
                    | e -> Extprot.Error.conversion_error e])
          >>
      | None -> readerf in
  let patt = patt_of_ll_type in
  let module C = Codec in
  fun t ->
    let x = match t with
    | Vint (Bool, _) -> Some (patt C.Bits8, reader_func `Read_raw_bool)
    | Vint (Int8, _) -> Some (patt C.Bits8, reader_func `Read_raw_i8)
    | Vint (Int, _) -> Some (patt C.Vint, reader_func `Read_raw_rel_int)
    | Bitstring32 _ -> Some (patt C.Bits32, reader_func `Read_raw_i32)
    | Bitstring64 (Long, _) -> Some (patt C.Bits64_long, reader_func `Read_raw_i64)
    | Bitstring64 (Float, _) -> Some (patt C.Bits64_float, reader_func `Read_raw_float)
    | Bytes _ -> Some (patt C.Bytes, reader_func `Read_raw_string)
    | Sum _ | Record _ | Tuple _ | Htuple _ | Message _ -> None
    in
    match x with
    | None -> None
    | Some (pat, reader) -> Some (pat, wrap t reader)


let messages_with_subsets bindings =
  SMap.fold
    (fun _ decl l -> match decl with
       | Message_decl (_, `Message_subset (name, _, _), _) -> name :: l
       | Message_decl
           (_, (`Message_record _ | `Message_alias _ |
                `Message_sum _ | `Message_app _), _)
       | Type_decl _ -> l)
    bindings []

let add_message_reader bindings msgname mexpr opts c =
  let _loc = Loc.mk "<generated code @ add_message_reader>" in
  let llrec = Gencode.low_level_msg_def bindings msgname mexpr in
  let module Mk_normal_reader =
    Make_reader(struct
                  let name = "STR"
                  let reader_func t =
                    <:expr< Extprot.Reader.String_reader.
                              $lid:Reader.string_of_reader_func t$ >>

                  let raw_rd_func = raw_rd_func reader_func

                  let read_msg_func = ((^) "read_")
                end) in

  let with_subsets = messages_with_subsets bindings in

  let field_readers, read_expr =
    Mk_normal_reader.read_message
      ~inline:(not @@ List.mem msgname with_subsets)
      msgname opts llrec
  in
    {
      c with c_reader =
         Some <:str_item<
                $field_readers$;

                value $lid:"read_" ^ msgname$ s = $read_expr$;

                value $lid:"io_read_" ^ msgname$ io =
                  $lid:"read_" ^ msgname$
                    (Extprot.Reader.String_reader.from_io_reader io);

                value $lid:"fast_io_read_" ^ msgname$ = $lid:"io_read_" ^ msgname$;

                value read = $lid:"read_" ^ msgname$;
                value io_read = $lid:"io_read_" ^ msgname$;
                value fast_io_read = $lid:"fast_io_read_" ^ msgname$;
              >>
    }

let add_message_io_reader bindings msgname mexpr opts c =
  let _loc = Loc.mk "<generated code @ add_message_io_reader>" in
  let llrec = Gencode.low_level_msg_def bindings msgname mexpr in
  let module Mk_io_reader =
    Make_reader(struct
                  let name = "IO"
                  let reader_func t =
                    <:expr< Extprot.Reader.IO_reader.
                              $lid:Reader.string_of_reader_func t$ >>

                  let raw_rd_func = raw_rd_func reader_func

                  let read_msg_func = ((^) "io_read_")
                end) in

  let with_subsets = messages_with_subsets bindings in

  let field_readers, ioread_expr =
    Mk_io_reader.read_message
      ~inline:(not @@ List.mem msgname with_subsets)
      msgname opts llrec
  in
    {
      c with c_io_reader =
        Some <:str_item<
                $field_readers$;
                value $lid:"io_read_" ^ msgname$ s = $ioread_expr$;
                value io_read = $lid:"io_read_" ^ msgname$;
             >>
    }

let rec write_field ?namespace fname =
  let _loc = Loc.mk "<generated code @ write>" in
  let simple_write_func = function
      Vint (Bool, _) -> "write_bool"
    | Vint (Int, _) -> "write_relative_int"
    | Vint (Int8, _) -> "write_int8"
    | Bitstring32 _ -> "write_int32"
    | Bitstring64 (Long, _) -> "write_int64"
    | Bitstring64 (Float, _) -> "write_float"
    | Bytes _ -> "write_string"
    | Tuple _ | Sum _ | Record _ | Htuple _ | Message _ -> assert false in

  let rec write_values tag var_tys =
    let nelms = List.length var_tys in
    let write_elms =
      List.map (fun (v, ty) -> write <:expr< $lid:v$ >> ty) var_tys
    in
      <:expr<
        let abuf =
          let aux = Extprot.Msg_buffer.create () in do {
            $Ast.exSem_of_list write_elms$;
            aux
          }
        in do {
          Extprot.Msg_buffer.add_tuple_prefix aux $int:string_of_int tag$;
          Extprot.Msg_buffer.add_vint aux
            (Extprot.Msg_buffer.length abuf +
             $int:string_of_int @@ Codec.vint_length nelms$);
          Extprot.Msg_buffer.add_vint aux $int:string_of_int nelms$;
          Extprot.Msg_buffer.add_buffer aux abuf;
          Extprot.Msg_buffer.discard abuf
        }
      >>

  and write_tuple tag v lltys =
    let var_tys = List.mapi (fun i ty -> (sprintf "v%d" i, ty)) lltys in
    let patt =
      Ast.paCom_of_list @@ List.map (fun (v, _) -> <:patt< $lid:v$ >>) var_tys
    in
      <:expr<
        let $patt$ = $v$ in
          $write_values tag var_tys$
      >>

  and write v = function
      Vint (_, opts) | Bitstring32 opts | Bitstring64 (_, opts)
    | Bytes opts as llty ->
        <:expr< Extprot.Msg_buffer.$lid:simple_write_func llty$
                  aux $wrap_value opts v$ >>
    | Message (path, name, _) ->
        let full_path = path @ [String.capitalize name] in
        let id = ident_with_path _loc full_path ("write_" ^ name) in
        <:expr< $id:id$ aux $v$ >>
    | Tuple (lltys, opts) -> write_tuple 0 (wrap_value opts v) lltys
    | Htuple (kind, llty, opts) ->
        let iter_f = match kind with
            Array -> <:expr< Array.iter >>
          | List -> <:expr< List.iter >>
        in
          <:expr<
            let write_elm aux v = $write <:expr< v >> llty$ in
            let nelms = ref 0 in
            let abuf = Extprot.Msg_buffer.create () in do {
                $iter_f$ (fun v -> do { write_elm abuf v; incr nelms } )
                         $wrap_value opts v$;
                Extprot.Msg_buffer.add_htuple_prefix aux 0;
                Extprot.Msg_buffer.add_vint aux
                  (Extprot.Msg_buffer.length abuf +
                   Extprot.Codec.vint_length nelms.contents);
                Extprot.Msg_buffer.add_vint aux nelms.contents;
                Extprot.Msg_buffer.add_buffer aux abuf;
                Extprot.Msg_buffer.discard abuf
              }
         >>
    | Sum (constructors, opts) ->
        let constant, non_constant = partition_constructors constructors in
        let constant_match_cases =
          List.map
            (fun c ->
               <:match_case<
                 $uid:String.capitalize c.const_type$.$lid:c.const_name$ ->
                   Extprot.Msg_buffer.add_const_prefix aux $int:string_of_int c.const_tag$
               >>)
            constant in
        let non_constant_cases =
          List.map
            (fun (c, lltys) ->
               let var_tys = List.mapi (fun i ty -> (sprintf "v%d" i, ty)) lltys in
               let patt =
                 List.map (fun (v, _) -> <:patt< $lid:v$ >>) var_tys |>
                 Ast.paCom_of_list
               in
                 <:match_case<
                   $uid:String.capitalize c.const_type$.$uid:c.const_name$
                     $patt$ -> $write_values c.const_tag var_tys$
                 >>)
            non_constant in
        let match_cases = constant_match_cases @ non_constant_cases in
          begin match get_type_info opts with
              None -> <:expr< match $v$ with [ $Ast.mcOr_of_list match_cases$ ] >>
            | Some { tof; _ } ->
                <:expr< match $tof$ $v$ with [ $Ast.mcOr_of_list match_cases$ ] >>
          end

    | Record (name, fields, opts) ->
        let fields' =
          List.map (fun f -> (f.field_name, true, f.field_type)) fields in
        let v = match get_type_info opts with
            None -> v
          | Some { tof; _ } -> <:expr< $tof$ $v$ >>
        in
          <:expr< let b = aux in
                  let msg = $ v $ in
                    $dump_fields ~namespace:name 0 fields'$ >>

  in match namespace with
      None -> write <:expr< msg.$lid:fname$ >>
    | Some ns -> write <:expr< msg.$uid:String.capitalize ns$.$lid:fname$ >>

and write_fields ?namespace fs =
  Ast.exSem_of_list @@ List.map (fun (name, _, llty) -> write_field ?namespace name llty) fs

and dump_fields ?namespace tag fields =
  let _loc = Loc.mk "<generated code @ dump_fields>" in
    let nelms = List.length fields in
      <:expr<
         let aux = Extprot.Msg_buffer.create () in
         let nelms = $int:string_of_int nelms$ in do {
           Extprot.Msg_buffer.add_tuple_prefix b  $int:string_of_int tag$;
           $write_fields ?namespace fields$;
           Extprot.Msg_buffer.add_vint b
             (Extprot.Msg_buffer.length aux +
              $int:string_of_int @@ Codec.vint_length nelms$);
           Extprot.Msg_buffer.add_vint b nelms;
           Extprot.Msg_buffer.add_buffer b aux;
           Extprot.Msg_buffer.discard aux
         }
      >>

and write_message msgname msg =
  ignore msgname;
  let _loc = Loc.mk "<generated code @ write_message>" in
    match msg with
      | Message_single (namespace, fields) -> Some (dump_fields ?namespace 0 fields)
      | Message_alias (path, name) ->
          let full_path = path @ [String.capitalize name] in
            Some <:expr< $id:ident_with_path _loc full_path ("write_" ^ name)$ b msg >>
      | Message_sum l ->
          let match_case (tag, ns, constr, fields) =
            <:match_case<
            $uid:constr$ msg ->
              $dump_fields ~namespace:(Option.default constr ns) tag fields$ >> in
          let match_cases =
            Ast.mcOr_of_list @@ List.map match_case @@
            List.mapi (fun i (ns, c, fs) -> (i, ns, c, fs)) l
          in Some <:expr< match msg with [ $match_cases$ ] >>
      | Message_subset _ -> None

let wrap_writer _loc opts expr = match get_type_info opts with
    Some { tof; _ } ->
      <:expr<
        try
          let msg = $tof$ msg in
            $expr$
        with [ Extprot.Error.Extprot_error _ as e -> raise e
             | e -> Extprot.Error.conversion_error e]
      >>
  | None -> expr

let add_message_writer bindings msgname mexpr opts c =
  let _loc = Loc.mk "<generated code @ add_message_writer>" in
  let llrec = Gencode.low_level_msg_def bindings msgname mexpr in
    match Option.map (wrap_writer _loc opts) @@ write_message msgname llrec with
      | None -> c
      | Some write_expr ->
          let writer =
            <:str_item< value $lid:"write_" ^ msgname$ b msg = $write_expr$;
                        value write = $lid:"write_" ^ msgname$; >>
          in { c with c_writer = Some writer }

let msgdecl_generators : (string * _ msgdecl_generator) list =
  [
    "reader", add_message_reader;
    "io_reader", add_message_io_reader;
    "writer", add_message_writer;
    "pretty_printer", Pretty_print.add_msgdecl_pretty_printer;
  ]

let typedecl_generators : (string * _ typedecl_generator) list =
  [
    "pretty_printer", Pretty_print.add_typedecl_pretty_printer;
  ]
