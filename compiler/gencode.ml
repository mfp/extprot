
open Ptypes
open ExtList
open Printf

include Gencode_types

let (|>) x f = f x

let failwithfmt fmt = kprintf (fun s -> if true then failwith s) fmt
let exit_with_error fmt =
  kprintf (fun s -> print_endline "Found 1 error:"; print_endline s; exit 1) fmt

let merge_options opt1 opt2 = opt2 @ opt1

let update_bindings bindings params args =
  List.fold_right2 SMap.add params args bindings

let must_keep_field subset ((name, mut, ev_regime, opts, ty) as field) = match subset with
  | Exclude_fields l ->
      if not @@ List.mem name l then Some (`Orig field) else None
  | Include_fields l ->
      try
        match List.assoc name l with
          | (None, None) -> Some (`Orig field)
          | (Some ty, None) -> Some (`Newtype (name, mut, ev_regime, opts, ty))
          | (None, Some ev_regime) -> Some (`Orig (name, mut, ev_regime, opts, ty))
          | (Some ty, Some ev_regime) -> Some (`Newtype (name, mut, ev_regime, opts, ty))
      with Not_found -> None

let subset_field = function
  | `Orig (name, mut, ev_regime, opts, ty)
  | `Newtype (name, mut, ev_regime, opts, ty) -> (name, mut, ev_regime, opts, ty)

let subset_of_selection selection = function
  | `Include -> Include_fields selection
  | `Exclude -> Exclude_fields (List.map fst selection)

let beta_reduce_base_texpr_aux f self (bindings : bindings) : base_type_expr -> 'a = function
  | #base_type_expr_simple as x -> x
  | `Tuple (l, opts) -> `Tuple (List.map (self bindings) l, opts)
  | `List (t, opts) -> `List (self bindings t, opts)
  | `Array (t, opts) -> `Array (self bindings t, opts)
  | (`App _ | `Ext_app _ | `Type_param _) as x -> f bindings x

let beta_reduce_sum self bindings s opts =
  let constructors =
    List.map
      (function
         | `Constant _ as x -> x
         | `Non_constant (const, tys) ->
           `Non_constant (const, List.map (self bindings) tys))
      s.constructors
  in `Sum ({ s with constructors }, opts)

let beta_reduce_record self bindings r opts =
  let fields' =
    List.map
      (fun (name, ismutable, ev_regime, opts, ty) -> (name, ismutable, ev_regime, opts, self bindings ty))
      r.record_fields
  in
  `Record ({r with record_fields = fields'}, opts)

let merge_rtexpr_options (rtexpr : reduced_type_expr) (opts : type_options)
      : reduced_type_expr =
  let m = merge_options in match rtexpr with
      `Array (t, opts2) -> `Array (t, m opts opts2)
    | `Bool opts2 -> `Bool (m opts opts2)
    | `Byte opts2 -> `Byte (m opts opts2)
    | `Float opts2 -> `Float (m opts opts2)
    | `Int opts2 -> `Int (m opts opts2)
    | `List (t, opts2) -> `List (t, m opts opts2)
    | `Long_int opts2 -> `Long_int (m opts opts2)
    | `Message (n, e, opts2) -> `Message (n, e, m opts opts2)
    | `Ext_message (p, n, opts2) -> `Ext_message (p, n, m opts opts2)
    | `Record (fs, opts2) -> `Record (fs, m opts opts2)
    | `String opts2 -> `String (m opts opts2)
    | `Sum (t, opts2) -> `Sum (t, m opts opts2)
    | `Tuple (l, opts2) -> `Tuple (l, m opts opts2)

let rec beta_reduce_texpr bindings (texpr : base_type_expr) : reduced_type_expr =
  let rec aux bindings x : reduced_type_expr =

    let reduce_texpr bindings = function
        `Sum _ | `Record _ | `Type_param _ | `App _ as x -> aux bindings x
        | #base_type_expr as x -> beta_reduce_texpr bindings x

    in match x with
      `Sum (s, opts) -> beta_reduce_sum beta_reduce_texpr bindings s opts
    | `Record (r, opts) -> beta_reduce_record beta_reduce_texpr bindings r opts
    | `Type_param p ->
        let name = string_of_type_param p in begin match smap_find name bindings with
          | Some (Message_decl (name, `Message_record l, _, opts)) ->
              let l =
                List.map
                  (fun (fname, fmut, fevr, fopts, fty) ->
                     (fname, fmut, fevr, fopts, beta_reduce_texpr bindings fty))
                  l
              in
                `Message (name, Some { record_name = name; record_fields = l }, opts)
          | Some (Message_decl (name, _, _, opts)) ->
              `Message (name, None, opts)
          | Some (Type_decl (_, [], exp, opts)) ->
              merge_rtexpr_options (reduce_texpr bindings exp) opts
          | Some (Type_decl (_, _, _, _)) ->
              failwithfmt "beta_reduce_texpr: wrong arity for higher-order type %S" name;
              assert false
          | None -> failwithfmt "beta_reduce_texpr: unbound type variable %S" name;
                    assert false
        end

    | `Ext_app (path, name, [], opts) -> `Ext_message (path, name, opts)
    | `Ext_app _ -> failwith "beta_reduce_texpr: external polymorphic types not supported"

    | `App (name, args, opts) -> match smap_find name bindings with
      | Some (Message_decl (_, `Message_record l, _, opts2)) ->
          let l =
            List.map
              (fun (fname, fmut, fevr, fopts, fty) ->
                 (fname, fmut, fevr, fopts, beta_reduce_texpr bindings fty))
              l
          in
            `Message (name, Some { record_name = name; record_fields = l }, merge_options opts opts2)
      | Some (Message_decl (_, _, _, opts2)) ->
          `Message (name, None, merge_options opts opts2)
      | Some (Type_decl (_, params, exp, opts2)) ->
          let bindings =
            update_bindings bindings (List.map string_of_type_param params)
              (List.map (fun ty -> Type_decl ("<bogus>", [], type_expr ty, [])) args) in
          let bindings, exp = alpha_convert bindings exp in
            merge_rtexpr_options (reduce_texpr bindings exp) opts2
      | None -> failwithfmt "beta_reduce_texpr: unbound type variable %S" name;
          assert false

  in beta_reduce_base_texpr_aux aux beta_reduce_texpr bindings texpr

and alpha_convert =
  let newid =
    let n = ref 0 in
      fun () -> sprintf "__extprot_%d" (incr n; !n) in

  fun bindings (exp : type_expr) ->

    let lookup bs p =
      let name = Type_param.string_of_type_param p in
        match smap_find name bs with
            Some x -> x
          | None -> failwithfmt "alpha_convert: unbound type variable %S" name;
                    assert false in

    let rec convert_base_texpr bindings : (base_type_expr as 'a) -> bindings * 'a =
      let self = convert_base_texpr in
      let convert_list f l =
        let bs, l =
          List.fold_right
            (fun ty (bs, l) -> let bs, ty = self bs ty in (bs, ty :: l))
            l (bindings, [])
        in f bs l

      in function
        `Type_param p ->
          let p' = type_param_of_string (newid ()) in
            (update_bindings bindings [string_of_type_param p'] [lookup bindings p],
             `Type_param p')
      | `App (s, tys, opts) -> convert_list (fun bs tys -> (bs, `App (s, tys, opts))) tys
      | `Tuple (tys, opts) -> convert_list (fun bs tys -> (bs, `Tuple (tys, opts))) tys
      | `List (ty, opts) -> let bs, ty = self bindings ty in (bs, `List (ty, opts))
      | `Array (ty, opts) -> let bs, ty = self bindings ty in (bs, `Array (ty, opts))
      | x -> (bindings, x)

    and convert_type_expr bindings : type_expr -> bindings * type_expr =
      let do_convert = convert_base_texpr in function
      | `Record (r, opts) ->
          let bs, fields =
            List.fold_right
              (fun (name, mut, ev_regime, opts, ty) (bs, l) ->
                 let bs, ty = do_convert bs ty in (bs, (name, mut, ev_regime, opts, ty) :: l))
              r.record_fields
              (bindings, [])
          in (bs, `Record ({ r with record_fields = fields }, opts))
      | `Sum (s, opts) ->
          let bs, constructors =
            List.fold_right
              (fun constructor (bs, l) ->
                 match constructor with
                   | `Constant _ as x -> (bs, x :: l)
                   | `Non_constant (cons, tys) ->
                       let bs, tys =
                         List.fold_right
                           (fun ty (bs, l) -> let bs, ty = do_convert bs ty in (bs, ty :: l))
                           tys (bs, [])
                       in (bs, `Non_constant (cons, tys) :: l))
              s.constructors
              (bindings, [])
          in (bs, `Sum ({ s with constructors }, opts))
      | #base_type_expr as x ->
          let bs, ty = convert_base_texpr bindings x in (bs, type_expr ty)

    in convert_type_expr bindings exp

let rec reduce_to_poly_texpr_core bindings (btexpr : base_type_expr) : poly_type_expr_core =
  let self = reduce_to_poly_texpr_core in

  let aux bindings x : poly_type_expr_core =
    match x with
    | `Type_param p -> `Type_arg (type_param_name p)
    | `Ext_app (path, name, args, opts) ->
        `Ext_type (path, name, List.map (self bindings) args, opts)
    | `App (name, args, opts) ->
        match smap_find name bindings with
        | Some (Message_decl (_, _, _, opts2)) -> `Type (name, [], [], merge_options opts opts2)
        | Some (Type_decl (name, params, _, opts2)) ->
            `Type (name, params, List.map (self bindings) args, merge_options opts opts2)
        | None -> `Type_arg name

  in beta_reduce_base_texpr_aux aux self bindings btexpr

let poly_beta_reduce_texpr bindings : type_expr -> poly_type_expr = function
    `Sum (s, opts) -> beta_reduce_sum reduce_to_poly_texpr_core bindings s opts
  | `Record (r, opts) -> beta_reduce_record reduce_to_poly_texpr_core bindings r opts
  | #base_type_expr as x -> (reduce_to_poly_texpr_core bindings x :> poly_type_expr)

let map_message bindings (f : base_type_expr -> _) g msgname (msg : message_expr) =
  let map_field f (fname, mutabl, ev_regime, opts, ty) = (fname, mutabl, ev_regime, opts, f ty) in

  let expand_record_type f name ty =
    match beta_reduce_texpr bindings ty with
      | `Record (r, opts)
      | `Message (_, Some r, opts) -> f r opts
      | x -> failwithfmt
               "Wrong type abbreviation in message %s : %S is %s, but only record type is allowed"
               msgname name (kind_of_reduced_type_expr x);
             assert false
  in
  match msg with
  | `Message_sum cases  ->
      Message_sum
        (List.map
           (function
                (const, `Message_record fields) -> (None, const, List.map (map_field f) fields)
              | (const, (`Message_app (name, _args, _opts))) ->
                  expand_record_type
                    (fun r _opts -> (Some name, const, List.map (map_field g) r.record_fields))
                    name (`App (name, _args, _opts)))
           cases)
  | `Message_record fields -> Message_single (None, List.map (map_field f) fields)
  | `Message_app (name, [], _opts) -> begin
      match smap_find name bindings with
        | Some (Message_decl (_, `Message_record l, _, _opts2)) ->
            Message_typealias (name, Some (List.map (map_field f) l))
        | _ -> Message_typealias (name, None)
    end
  | `Message_app (name, _args, _opts) ->
      expand_record_type
        (fun r _opts ->
            Message_single (Some r.record_name, List.map (map_field g) r.record_fields))
        name (`App (name, _args, _opts))
  | `Message_alias (path, name) -> Message_alias (path, name)
  | `Message_subset (name, l, sign) ->
      let subset =
        match sign with
          | `Include -> Include_fields (List.map (fun (n, (ty, evr)) -> (n, (Option.map f ty, evr))) l)
          | `Exclude -> Exclude_fields (List.map fst l) in

      let rec find_orig_def name bindings =
        match smap_find name bindings with
          | Some (Message_decl (_, `Message_app (name, [], _), _, _)) as x -> begin
              match smap_find name bindings with
                | Some (Message_decl (_, `Message_record _, _, _)) ->
                    find_orig_def name bindings
                | _ -> x
            end
          | x -> x
      in
        match find_orig_def name bindings with
          | Some (Message_decl (name, `Message_record fields, _, _opts)) ->
              Message_subset (name, List.map (map_field f) fields, subset)

          | Some (Message_decl (_, `Message_app (name_, args_, opts), _, opts_))
          | Some (Type_decl (name_, ([] as args_), `Record _, (opts as opts_))) -> begin
              match
                expand_record_type
                  (fun r _opts ->
                     Message_single (None, List.map (map_field g) r.record_fields))
                  name (`App (name_, args_, merge_options opts_ opts))
              with
                | Message_single (_, fields) ->
                    Message_subset (name, fields, subset)
                | _ ->
                    failwithfmt
                      "wrong message subset %s: source %s is not a simple message"
                      name name_;
                    assert false
            end
          | None | Some _ ->
              failwithfmt
                "wrong message subset: %s is not a simple message" name;
              assert false

let iter_message bindings f g msgname msg =
  let proc_field f const (fname, mutabl, _ev_regime, _opts, ty) = f const fname mutabl ty in

  let iter_expanded_type const name ty =
    match beta_reduce_texpr bindings ty with
          | `Record (r, _opts)
          | `Message (_, Some r, _opts) ->
              List.iter (proc_field g const) r.record_fields
          | x ->
              failwithfmt
                "Wrong type abbreviation in message %s : %S is %s, but only record type is allowed"
                 msgname name (kind_of_reduced_type_expr x);
              assert false
  in
  match msg with
  | `Message_sum cases  ->
        List.iter
          (function
               (const, `Message_record fields) -> List.iter (proc_field f const) fields
             | (const, (`Message_app (name, _args, _opts))) ->
                 iter_expanded_type const name (`App (name, _args, _opts)))
          cases
  | `Message_record fields -> List.iter (proc_field f "") fields
  | `Message_app(name, _args, _opts) ->
      iter_expanded_type "" name (`App (name, _args, _opts))

let beta_reduced_msg_app_fields bindings name args =

  let rec resolve_texpr_type_vars bindings expr =
    let self = resolve_texpr_type_vars bindings in

      match expr with
        | `App (name, args, opts) -> `App (name, List.map self args, opts)

        | `Ext_app (path, name, args, opts) ->
            `Ext_app (path, name, List.map self args, opts)

        | `Type_param param as x -> begin
            match smap_find (string_of_type_param param) bindings with
              | None -> x
              | Some ty -> ty
          end

        | `Tuple (tys, opts) -> `Tuple (List.map self tys, opts)
        | `List (ty, opts) -> `List (self ty, opts)
        | `Array (ty, opts) -> `Array (ty, opts)
        | #base_type_expr_simple as x -> x

  in

    match smap_find name bindings with
      | Some (Type_decl (_, params, `Record (r, _), _)) -> begin

          let bs =
            List.fold_left2
              (fun b name ty -> SMap.add (string_of_type_param name) ty b)
              SMap.empty params args in

          let record_fields =
            List.map
              (fun (name, mut, ev_regime, opts, ty) ->
                 (name, mut, ev_regime, opts, resolve_texpr_type_vars bs ty))
              r.record_fields
          in
            Some (bindings, record_fields)
        end
      | Some (Message_decl (_, `Message_record l, _, _)) ->
          Some (bindings, l)
      | _ -> None

let rec low_level_msg_def bindings msgname (msg : message_expr) =

  let rec low_level_of_rtexp : reduced_type_expr -> low_level = function
      `Bool opts -> Vint (Bool, opts)
    | `Byte opts -> Vint (Int8, opts)
    | `Int opts -> Vint (Int, opts)
    | `Long_int opts -> Bitstring64 (Long, opts)
    | `Float opts -> Bitstring64 (Float, opts)
    | `String opts -> Bytes opts
    | `Tuple (l, opts) -> Tuple (List.map low_level_of_rtexp (l :> reduced_type_expr list), opts)
    | `List (ty, opts) -> Htuple (List, low_level_of_rtexp (reduced_type_expr ty), opts)
    | `Array (ty, opts) -> Htuple (Array, low_level_of_rtexp (reduced_type_expr ty), opts)
    | `Ext_message (path, s, opts) -> Message(path, s, None, opts)
    | `Message (s, _, opts) -> begin
        match smap_find s bindings with
          | None | Some (Type_decl _) -> Message ([], s, None, opts)
          | Some (Message_decl (_, mexpr, _, _)) ->
              let llmdef = low_level_msg_def bindings s mexpr in
                Message ([], s, Some llmdef, opts)
      end
    | `Record (r, opts) ->
        let fields =
          List.map
            (fun (name, _ismutable, ev_regime, opts, ty) ->
               { field_name = name; field_type = low_level_of_rtexp ty;
                 field_evr  = ev_regime;
                 field_opts = opts;
               })
            r.record_fields
        in Record (r.record_name, fields, opts)
    | `Sum (sum, opts) ->
        let const_idx    = ref 0 in
        let nonconst_idx = ref 0 in

        let constructors =
          List.map
            (function
               | `Constant const_name ->
                   let const_tag = !const_idx in
                     incr const_idx;
                     `Constant { const_tag; const_name; const_type = sum.type_name }
               | `Non_constant (const_name, tys) ->
                   let const_tag = !nonconst_idx in
                     incr nonconst_idx;
                     `Non_constant
                       ({ const_tag; const_name; const_type = sum.type_name},
                        List.map low_level_of_rtexp tys))
            sum.constructors
        in Sum (constructors, opts) in

  let low_level_field ty = beta_reduce_texpr bindings ty |> low_level_of_rtexp
  in
    map_message bindings low_level_field low_level_of_rtexp msgname msg

let decl_name = function
  | Message_decl (n, _, _, _) | Type_decl (n, _, _, _) -> n

let remove_type_opts o =
  List.filter (fun (k, _) -> k <> "ocaml.type") o

let replace_monomorphic_record_types_with_messages bindings =
  let rec replace_one bindings = function
    | Message_decl _ as x -> (x, bindings)
    | Type_decl (_, _ :: _, _, _) as x -> (x, bindings)
    | Type_decl (name, [], `Record (r, o1), o2) -> begin
        let mexpr = `Message_record r.record_fields in
        let opts  = merge_options o2 o1 in
          (Message_decl (name, mexpr, Export_NO, remove_type_opts opts), bindings)
      end
    | Type_decl (name, [], `App (name', [], o1), o2) as x -> begin
        match smap_find name' bindings with
          | None -> (x, bindings)
          | Some decl ->
              let decl, bindings = replace_one bindings decl in
                match decl with
                  | Message_decl (_, mexpr, export, o3) ->
                      (Message_decl
                         (name, mexpr, export,
                          remove_type_opts @@
                          merge_options o2 @@ merge_options o1 o3),
                       bindings)
                  | x -> (x, bindings)
      end

    | Type_decl _ as x -> (x, bindings)
  in
    List.fold_left
      (fun bindings (k, _) ->
         match smap_find k bindings with
           | None -> bindings
           | Some decl ->
               let decl, bindings = replace_one bindings decl in
                 SMap.add (decl_name decl) decl bindings)
      bindings (SMap.bindings bindings)

let collect_bindings =
  List.fold_left (fun m decl -> SMap.add (declaration_name decl) decl m) SMap.empty

type 'container msgdecl_generator =
    export:bool -> bindings -> string -> message_expr -> type_options ->
    'container -> 'container

type 'container typedecl_generator =
    bindings -> string -> type_param list -> type_expr -> type_options ->
    'container -> 'container

module type GENCODE =
sig
  type toplevel
  type container
  type entry = Toplevel of (toplevel * string) | Container of container

  val generate_include : string -> toplevel * string
  val generate_container : bindings -> declaration -> container option
  val msgdecl_generators : (string * container msgdecl_generator) list
  val typedecl_generators : (string * container typedecl_generator) list
  val generate_code :
    ?global_opts:(string * string) list -> ?width:int -> entry list -> string * string
end

let (|>) x f = f x

module Make(Gen : GENCODE) =
struct
  open Gen

  let generators =
    let names l = List.map fst l in
      names Gen.typedecl_generators @ names Gen.msgdecl_generators |>
        List.unique |> List.sort

  let generate_code ?width ?(generators : string list option) ?(global_opts = []) bindings entries =

    let bindings = replace_monomorphic_record_types_with_messages bindings in

    let entries =
      List.map
        (function
          | Include _ as x -> x
          | Decl x ->
              match smap_find (decl_name x) bindings with
                | None -> Decl x
                | Some x -> Decl x)
        entries in

    let use_generator name = match generators with
        None -> true
      | Some l -> List.mem name l in

    entries |>
      List.filter_map begin function
      | Include file -> Some (Toplevel (generate_include file))
      | Decl decl ->
        generate_container bindings decl |>
          Option.map begin fun cont ->
           let container =
             match decl with
             | Type_decl (name, params, expr, opts) -> begin
                 List.fold_left
                   (fun cont (gname, f) ->
                      if use_generator gname then
                        f bindings name params expr (opts @ global_opts) cont
                      else cont)
                   cont
                   Gen.typedecl_generators
               end
             | Message_decl (name, expr, export, opts) ->
                 List.fold_left
                   (fun cont (gname, f) ->
                      if use_generator gname then
                        f ~export:(match export with Export_YES -> true | Export_NO -> false)
                          bindings name expr (opts @ global_opts) cont
                      else cont)
                   cont
                   Gen.msgdecl_generators
           in
             Container container
          end
      end |>
      Gen.generate_code ~global_opts ?width
end

module Prettyprint =
struct
  open Format
  let pp = fprintf
  let pp' fmt ppf = pp ppf fmt

  let list elt sep ppf =
    let rec loop = function
        [] -> ()
      | x::xs -> pp ppf sep; elt ppf x; loop xs
    in function
        [] -> ()
      | [x] -> elt ppf x
      | x::xs -> elt ppf x; loop xs

  let pp_base_expr_simple ppf : base_type_expr_simple -> unit = function
      `Bool _ -> pp ppf "Bool"
    | `Byte _ -> pp ppf "Byte"
    | `Int _ -> pp ppf "Int"
    | `Long_int _ -> pp ppf "Long_int"
    | `Float _ -> pp ppf "Float"
    | `String _ -> pp ppf "String"

  let pp_base_type_expr_core f ppf : 'a base_type_expr_core -> unit = function
      `Tuple (l, _) -> pp ppf "@[<1>(%a)@]" (list f " *@ ") l
    | `List (t, _) -> pp ppf "@[<1>[%a]@]" f t
    | `Array (t, _) -> pp ppf "@[<1>[|%a|]@]" f t
    | #base_type_expr_simple as x -> pp_base_expr_simple ppf x

  let rec pp_reduced_type_expr ppf : reduced_type_expr -> unit = function
      `Sum (s, _) -> begin match (Protocol_types.non_constant_constructors s) with
          [] -> pp ppf "@[<1>%a@]" (list (pp' "%s") "@ | ")
                  (Protocol_types.constant_constructors s)
        | non_const ->
            let pp_non_constant ppf cases =
              let elt ppf (const, l) =
                pp ppf "%s @[<1>(%a)@]" const (list pp_reduced_type_expr "@ ") l
              in list elt "@ | " ppf cases
            in match Protocol_types.constant_constructors s with
                [] -> pp ppf "@[<1>%a@]" pp_non_constant non_const
              | l -> pp ppf "@[<1>%a@ | %a@]" (list (pp' "%s") "@ | ") l
                       pp_non_constant non_const
      end
    | `Record (r, _) ->
        let pp_field ppf (name, ismutable, ev_regime, _opts, expr) =
          if ismutable then pp ppf "mutable ";
          begin
            match ev_regime with
              | `Eager -> ()
              | `Lazy -> pp ppf "lazy "
              | `Auto -> pp ppf "auto "
          end;
          pp ppf "@[<1>%s :@ %a@]" name pp_reduced_type_expr expr
        in pp ppf "@[<1>%a@]" (list pp_field ";@ ") r.record_fields
    | `Message (s, _, _) -> pp ppf "msg:%S" s
    | `Ext_message (path, s, _) -> pp ppf "msg:%S"
                                     (String.concat "." (path @ [s]))
    | #base_type_expr_core as x ->
        pp_base_type_expr_core pp_reduced_type_expr ppf x

  let inspect_base_type_expr_core f ppf : 'a base_type_expr_core -> unit = function
      `Tuple (l, _) -> pp ppf "`Tuple [@[<1> %a@ ]@]" (list f ",@ ") l
    | `List (t, _) -> pp ppf "`List @[%a@]" f t
    | `Array (t, _) -> pp ppf "`Array @[%a@]" f t
    | #base_type_expr_simple as x -> pp_base_expr_simple ppf x

  let pp_non_constant f ppf l =
    list
      (fun ppf (s, xs) -> pp ppf "@[%S, [@[<1> %a ]@]@]" s (list f ",@ ") xs)
      ";@ "
      ppf l

  let inspect_sum_dtype f ppf s =
    pp ppf "{@[<1>@ type_name = %S;@ constant = [@[<1>@ %a ]@];@ non_constant = [@[<1> %a ]@] }@]"
      s.type_name
      (list (pp' "%S") ",@ ") (Protocol_types.constant_constructors s)
      (pp_non_constant f) (Protocol_types.non_constant_constructors s)

  let string_of_eval_regime = function
    | `Eager -> "eager"
    | `Lazy -> "lazy"
    | `Auto -> "auto"

  let pp_fields f ppf l =
    list
      (fun ppf (name, mut, ev_regime, _opts, ty) ->
         pp ppf "@[%S,@ %s,@ %s,@ %a@]" name
           (string_of_bool mut) (string_of_eval_regime ev_regime) f ty)
      ",@ " ppf l

  let inspect_record_dtype f ppf r =
    pp ppf "{@[<1>@ record_name = %S;@ record_fields = [@[<1>@ %a ]@] }@]"
      r.record_name
      (pp_fields f) r.record_fields

  let rec inspect_reduced_type_expr ppf : reduced_type_expr -> unit = function
    | `Message (s, _, _) -> pp ppf "`Message %S" s
    | `Ext_message (path, s, _) -> pp ppf "`Ext_message %S" (String.concat "." (path @ [s]))
    | `Sum (s, _) -> pp ppf "@[<2>`Sum@ %a@]" (inspect_sum_dtype inspect_reduced_type_expr) s
    | `Record (r, _) -> pp ppf "@[<2>`Record@ %a@]" (inspect_record_dtype inspect_reduced_type_expr) r
    | #base_type_expr_core as x ->
        inspect_base_type_expr_core inspect_reduced_type_expr ppf x
end
