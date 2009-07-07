
open Ptypes
open ExtList
open Printf

let (|>) x f = f x

type tag = int

type low_level =
    Vint of vint_meaning * type_options
  | Bitstring32 of type_options
  | Bitstring64 of b64_meaning * type_options
  | Bytes of type_options
  | Sum of constructor list * (constructor * low_level list) list * type_options
  | Record of string * field list * type_options
  | Tuple of low_level list * type_options
  | Htuple of htuple_meaning * low_level * type_options
  | Message of string * type_options

and constructor = {
  const_tag : tag;
  const_name : string;
  const_type : string;
}

and field = {
  field_name : string;
  field_type : low_level;
}

and 'a message =
  | Record_single of string option * (string * bool * 'a) list
  | Record_sum of (string * (string * bool * 'a) list) list

and vint_meaning =
    Bool
  | Int8
  | Int

and b64_meaning =
    Long
  | Float

and htuple_meaning =
    List
  | Array

type reduced_type_expr = [
    reduced_type_expr base_type_expr_core
  | `Sum of reduced_type_expr sum_data_type * type_options
  | `Record of reduced_type_expr record_data_type * type_options
  | `Message of string * type_options
]

type poly_type_expr_core = [
    poly_type_expr_core base_type_expr_core
  | `Type of string * poly_type_expr_core list * type_options (* polymorphic sum type name, type args *)
  | `Type_arg of string
]

type poly_type_expr = [
  poly_type_expr_core
  | `Record of poly_type_expr_core record_data_type * type_options
  | `Sum of poly_type_expr_core sum_data_type * type_options
]

let reduced_type_expr e = (e :> reduced_type_expr)

type bindings = declaration SMap.t

let failwithfmt fmt = kprintf (fun s -> if true then failwith s) fmt

let merge_options opt1 opt2 = opt2 @ opt1

let update_bindings bindings params args =
  List.fold_right2 SMap.add params args bindings

let beta_reduce_base_texpr_aux f self (bindings : bindings) : base_type_expr -> 'a = function
  | #base_type_expr_simple as x -> x
  | `Tuple (l, opts) -> `Tuple (List.map (self bindings) l, opts)
  | `List (t, opts) -> `List (self bindings t, opts)
  | `Array (t, opts) -> `Array (self bindings t, opts)
  | (`App _ | `Type_param _) as x -> f bindings x

let beta_reduce_sum self bindings s opts =
  let non_const =
    List.map
      (fun (const, tys) ->
         (const, List.map (self bindings) tys))
      s.non_constant
  in `Sum ({ s with non_constant = non_const }, opts)

let rec beta_reduce_record self bindings r opts =
  let fields' =
    List.map
      (fun (name, ismutable, ty) -> (name, ismutable, self bindings ty))
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
    | `List (t, opts2) -> `List (t, opts2)
    | `Long_int opts2 -> `Long_int (m opts opts2)
    | `Message (n, opts2) -> `Message (n, m opts opts2)
    | `Record (fs, opts2) -> `Record (fs, m opts opts)
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
            Some (Message_decl (name, _, opts)) -> `Message (name, opts)
          | Some (Type_decl (_, [], exp, opts)) ->
              merge_rtexpr_options (reduce_texpr bindings exp) opts
          | Some (Type_decl (_, _, _, _)) ->
              failwithfmt "beta_reduce_texpr: wrong arity for higher-order type %S" name;
              assert false
          | None -> failwithfmt "beta_reduce_texpr: unbound type variable %S" name;
                    assert false
        end

    | `App (name, args, opts) -> match smap_find name bindings with
          Some (Message_decl (_, _, opts2)) -> `Message (name, merge_options opts opts2)
        | Some (Type_decl (_, params, exp, opts2)) ->
            let opts = merge_options opts opts2 in
            let bindings =
              update_bindings bindings (List.map string_of_type_param params)
                (List.map (fun ty -> Type_decl ("<bogus>", [], type_expr ty, opts)) args) in
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
      let self = convert_base_texpr in function
        `Type_param p ->
          let p' = type_param_of_string (newid ()) in
            (* printf "alpha %S -> %S\n%!" (string_of_type_param p) (string_of_type_param p'); *)
            (update_bindings bindings [string_of_type_param p'] [lookup bindings p],
             `Type_param p')
      | `App (s, l, opts) ->
          let bs, l = List.fold_right
                        (fun ty (bs, l) -> let bs, ty = self bs ty in (bs, ty :: l))
                        l
                        (bindings, [])
          in (bs, `App (s, l, opts))
      | `Tuple (tys, opts) ->
          let bs, tys =
            List.fold_right
              (fun ty (bs, tys) -> let bs, ty = self bs ty in (bs, ty :: tys))
              tys (bindings, [])
          in (bs, `Tuple (tys, opts))
      | `List (ty, opts) -> let bs, ty = self bindings ty in (bs, `List (ty, opts))
      | `Array (ty, opts) -> let bs, ty = self bindings ty in (bs, `Array (ty, opts))
      | x -> (bindings, x)

    and convert_type_expr bindings : type_expr -> bindings * type_expr =
      let do_convert = convert_base_texpr in function
      | `Record (r, opts) ->
          let bs, fields =
            List.fold_right
              (fun (name, mut, ty) (bs, l) ->
                 let bs, ty = do_convert bs ty in (bs, (name, mut, ty) :: l))
              r.record_fields
              (bindings, [])
          in (bs, `Record ({ r with record_fields = fields }, opts))
      | `Sum (s, opts) ->
          let bs, non_constant =
            List.fold_right
              (fun (cons, tys) (bs, l) ->
                 let bs, tys =
                   List.fold_right
                     (fun ty (bs, l) -> let bs, ty = do_convert bs ty in (bs, ty :: l))
                     tys (bs, [])
                 in (bs, (cons, tys) :: l))
              s.non_constant
              (bindings, [])
          in (bs, `Sum ({ s with non_constant = non_constant }, opts))
      | #base_type_expr as x ->
          let bs, ty = convert_base_texpr bindings x in (bs, type_expr ty)

    in convert_type_expr bindings exp

let rec reduce_to_poly_texpr_core bindings (btexpr : base_type_expr) : poly_type_expr_core =
  let self = reduce_to_poly_texpr_core in

  let aux bindings x : poly_type_expr_core = match x with
    | `Type_param p -> `Type_arg (type_param_name p)
    | `App (name, args, opts) -> match smap_find name bindings with
          Some (Message_decl (_, _, opts2)) -> `Type (name, [], merge_options opts opts2)
        | Some (Type_decl (name, _, `Sum _, opts2)) ->
            `Type (name, List.map (self bindings) args, merge_options opts opts2)
        | Some (Type_decl (name, _, _, opts2)) ->
            `Type (name, List.map (self bindings) args, merge_options opts opts2)
            (* let bindings = *)
              (* update_bindings bindings (List.map string_of_type_param params) *)
                (* (List.map (fun ty -> Type_decl ("<bogus>", [], type_expr ty)) args) *)
            (* in self bindings exp *)
        | None -> `Type_arg name

  in beta_reduce_base_texpr_aux aux self bindings btexpr

let poly_beta_reduce_texpr bindings : type_expr -> poly_type_expr = function
    `Sum (s, opts) -> beta_reduce_sum reduce_to_poly_texpr_core bindings s opts
  | `Record (r, opts) -> beta_reduce_record reduce_to_poly_texpr_core bindings r opts
  | #base_type_expr as x -> (reduce_to_poly_texpr_core bindings x :> poly_type_expr)

let rec map_message bindings (f : base_type_expr -> _) g =
  let map_field f (fname, mutabl, ty) = (fname, mutabl, f ty) in
  function
    `Sum cases  ->
      Record_sum
        (List.map
           (fun (const, `Record fields) -> (const, List.map (map_field f) fields))
           cases)
  | `Record fields -> Record_single (None, List.map (map_field f) fields)
  | `App(name, args, opts) as ty ->
      match beta_reduce_texpr bindings ty with
          `Record (r, opts) ->
            Record_single (Some r.record_name, List.map (map_field g) r.record_fields)
        | _ ->
            failwithfmt
              "Wrong type abbreviation in message (%S): must be a record type."
              name;
            assert false

let rec iter_message bindings f g =
  let proc_field f const (fname, mutabl, ty) = f const fname mutabl ty in
  function
    `Sum cases  ->
        List.iter
          (fun (const, `Record fields) -> List.iter (proc_field f const) fields)
          cases
  | `Record fields -> List.iter (proc_field f "") fields
  | `App(name, args, opts) as ty ->
      match beta_reduce_texpr bindings ty with
          `Record (r, opts) ->
            List.iter (proc_field g "") r.record_fields
        | _ ->
            failwithfmt
              "Wrong type abbreviation in message (%S): must be a record type."
              name;
            assert false

let low_level_msg_def bindings (msg : message_expr) =

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
    | `Message (s, opts) -> Message (s, opts)
    | `Record (r, opts) ->
        let fields =
          List.map
            (fun (name, ismutable, ty) ->
               { field_name = name; field_type = low_level_of_rtexp ty; })
            r.record_fields
        in Record (r.record_name, fields, opts)
    | `Sum (sum, opts) ->
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
        in Sum (constant, non_constant, opts) in

  let low_level_field ty = beta_reduce_texpr bindings ty |> low_level_of_rtexp
  in
    map_message bindings low_level_field low_level_of_rtexp msg

let collect_bindings =
  List.fold_left (fun m decl -> SMap.add (declaration_name decl) decl m) SMap.empty

type 'container msgdecl_generator =
    bindings -> string -> message_expr -> type_options ->
    'container -> 'container

type 'container typedecl_generator =
    bindings -> string -> type_param list -> type_expr -> type_options ->
    'container -> 'container

module type GENCODE =
sig
  type container

  val generate_container : bindings -> declaration -> container option
  val msgdecl_generators : (string * container msgdecl_generator) list
  val typedecl_generators : (string * container typedecl_generator) list
  val generate_code : container list -> string
end

let (|>) x f = f x

module Make(Gen : GENCODE) =
struct
  open Gen

  let generators =
    let names l = List.map fst l in
      names Gen.typedecl_generators @ names Gen.msgdecl_generators |>
        List.unique |> List.sort

  let generate_code ?(generators : string list option) (decls : declaration list) =
    let use_generator name = match generators with
        None -> true
      | Some l -> List.mem name l in

    let bindings = collect_bindings decls in
      decls |>
      List.filter_map begin fun decl ->
        generate_container bindings decl |>
          Option.map begin fun cont ->
            match decl with
               Type_decl (name, params, expr, opts) ->
                 List.fold_left
                   (fun cont (gname, f) ->
                      if use_generator gname then f bindings name params expr opts cont
                      else cont)
                   cont
                   Gen.typedecl_generators
             | Message_decl (name, expr, opts) ->
                 List.fold_left
                   (fun cont (gname, f) ->
                      if use_generator gname then f bindings name expr opts cont
                      else cont)
                   cont
                   Gen.msgdecl_generators
          end
      end |>
      Gen.generate_code
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
      `Sum (s, _) -> begin match s.non_constant with
          [] -> pp ppf "@[<1>%a@]" (list (pp' "%s") "@ | ") s.constant
        | non_const ->
            let pp_non_constant ppf cases =
              let elt ppf (const, l) =
                pp ppf "%s @[<1>(%a)@]" const (list pp_reduced_type_expr "@ ") l
              in list elt "@ | " ppf cases
            in match s.constant with
                [] -> pp ppf "@[<1>%a@]" pp_non_constant non_const
              | _ -> pp ppf "@[<1>%a@ | %a@]" (list (pp' "%s") "@ | ") s.constant
                       pp_non_constant non_const
      end
    | `Record (r, _) ->
        let pp_field ppf (name, ismutable, expr) =
          if ismutable then pp ppf "mutable ";
          pp ppf "@[<1>%s :@ %a@]" name pp_reduced_type_expr expr
        in pp ppf "@[<1>%a@]" (list pp_field ";@ ") r.record_fields
    | `Message (s, _) -> pp ppf "msg:%S" s
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
      (list (pp' "%S") ",@ ") s.constant
      (pp_non_constant f) s.non_constant

  let pp_fields f ppf l =
    list
      (fun ppf (name, mut, ty) ->
         pp ppf "@[%S,@ %s,@ %a@]" name (string_of_bool mut) f ty)
      ",@ " ppf l

  let inspect_record_dtype f ppf r =
    pp ppf "{@[<1>@ record_name = %S;@ record_fields = [@[<1>@ %a ]@] }@]"
      r.record_name
      (pp_fields f) r.record_fields

  let rec inspect_reduced_type_expr ppf : reduced_type_expr -> unit = function
    | `Message (s, _) -> pp ppf "`Message %S" s
    | `Sum (s, _) -> pp ppf "@[<2>`Sum@ %a@]" (inspect_sum_dtype inspect_reduced_type_expr) s
    | `Record (r, _) -> pp ppf "@[<2>`Record@ %a@]" (inspect_record_dtype inspect_reduced_type_expr) r
    | #base_type_expr_core as x ->
        inspect_base_type_expr_core inspect_reduced_type_expr ppf x
end
