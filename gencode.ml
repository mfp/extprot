
open Ptypes
open ExtList
open Printf

type tag = int

type low_level =
    Vint of vint_meaning
  | Bitstring32
  | Bitstring64 of b64_meaning
  | Bytes
  | Sum of constructor list * (constructor * low_level list) list
  | Tuple of low_level list
  | Htuple of htuple_meaning * low_level
  | Message of string

and constructor = {
  const_tag : tag;
  const_name : string;
  const_type : string;
}

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

type reduced_type_expr = [
    reduced_type_expr base_type_expr_core
  | `Sum of reduced_type_expr sum_data_type
  | `Message of string
]

type poly_type_expr_core = [
    poly_type_expr_core base_type_expr_core
  | `Type of string * poly_type_expr_core list (* polymorphic sum type name, type args *)
  | `Type_arg of string
]

type poly_type_expr = [
  poly_type_expr_core
  | `Sum of poly_type_expr_core sum_data_type
]

let reduced_type_expr e = (e :> reduced_type_expr)

type bindings = declaration SMap.t

let failwithfmt fmt = kprintf (fun s -> if true then failwith s) fmt

let update_bindings bindings params args =
  List.fold_right2 SMap.add params args bindings

let rec beta_reduce_aux sumf appf self (bindings : bindings) = function
  | #base_type_expr_simple as x -> x
  | `Sum s -> sumf bindings s
  | `Tuple l -> `Tuple (List.map (self bindings) (l :> type_expr list))
  | `List t -> `List (self bindings (type_expr t))
  | `Array t -> `Array (self bindings (type_expr t))
  | `App _ as app -> appf bindings app

let beta_reduce_sum self bindings s =
  let non_const =
    List.map
      (fun (const, tys) ->
         (const, List.map (self bindings) (tys :> type_expr list)))
      s.non_constant
  in `Sum { s with non_constant = non_const }

let rec beta_reduce_texpr bindings texpr : reduced_type_expr =
  let reduce_app _ (`App (name, args)) = match smap_find name bindings with
        Some (Message_decl _) -> `Message name
      | Some (Type_decl (name, params, exp)) ->
          let bindings =
            update_bindings bindings params
              (List.map (fun ty -> Type_decl ("<bogus>", [], type_expr ty)) args)
          in beta_reduce_texpr bindings exp
      | None -> failwithfmt "beta_reduce_texpr: unbound type variable %S" name;
                assert false
  in beta_reduce_aux
       (beta_reduce_sum beta_reduce_texpr) reduce_app beta_reduce_texpr
       bindings texpr

let poly_beta_reduce_texpr bindings (texpr : type_expr) : poly_type_expr =
  let rec reduce bindings (texpr : type_expr) : poly_type_expr_core =

    let reduce_sum bindings s =
      (* `Type (s.type_name, []) in *)
      (* there shouldn't be any of these left *)
      assert false in

    let reduce_app bindings (`App (name, args)) : poly_type_expr_core =
      match smap_find name bindings with
          Some (Message_decl _) -> `Type (name, [])
        | Some (Type_decl (name, params, `Sum _)) ->
            `Type (name, List.map (reduce bindings) (args :> type_expr list))
        | Some (Type_decl (name, params, exp)) ->
            let bindings =
              update_bindings bindings params
                (List.map (fun ty -> Type_decl ("<bogus>", [], type_expr ty)) args)
            in reduce bindings exp
        | None -> `Type_arg name

    in beta_reduce_aux reduce_sum reduce_app reduce bindings texpr

   (* must expand top-level `Sum!
    * otherwise the expr for type foo = A | B becomes `Type foo *)
  in match texpr with
        `Sum s -> beta_reduce_sum reduce bindings s
      | s -> (reduce bindings s :> poly_type_expr)


let low_level_msg_def bindings (msg : message_expr) =

  let rec low_level_of_rtexp : [reduced_type_expr] -> low_level = function
      `Bool -> Vint Bool
    | `Byte -> Vint Positive_int
    | `Int true -> Vint Positive_int
    | `Int false -> Vint Int
    | `Long_int -> Bitstring64 Long
    | `Float -> Bitstring64 Float
    | `String -> Bytes
    | `Tuple l -> Tuple (List.map low_level_of_rtexp (l :> reduced_type_expr list))
    | `List ty -> Htuple (List, low_level_of_rtexp (reduced_type_expr ty))
    | `Array ty -> Htuple (Array, low_level_of_rtexp (reduced_type_expr ty))
    | `Message s -> Message s
    | `Sum sum ->
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
        in Sum (constant, non_constant) in

  let rec low_level_of_mexpr : message_expr -> low_level_record =
    let low_level_field (const, mutabl, ty) =
      (const, mutabl, low_level_of_rtexp (beta_reduce_texpr bindings (type_expr ty)))

    in function
      `Sum cases  ->
        Record_sum
          (List.map
             (fun (const, `Record fields) -> (const, List.map low_level_field fields))
             cases)
    | `Record fields -> Record_single (List.map low_level_field fields)

  in low_level_of_mexpr msg

let collect_bindings =
  List.fold_left (fun m decl -> SMap.add (declaration_name decl) decl m) SMap.empty

module type GENCODE =
sig
  type container

  val generate_container : declaration -> container option
  val add_message_reader : string -> message_expr -> container -> container
  val add_message_writer : string -> message_expr -> container -> container
  val generate_code : container list -> string
end

let (|>) x f = f x

module Make(Gen : GENCODE) =
struct
  open Gen

  let generate_code (decls : declaration list) =
    List.filter_map
      (fun decl ->
         match generate_container decl with
             None -> None
           | Some cont ->
               match decl with
                   Type_decl _ -> Some cont
                 | Message_decl (name, expr) ->
                     Some (add_message_reader name expr cont |>
                             add_message_writer name expr))
      decls
    |> generate_code
end
