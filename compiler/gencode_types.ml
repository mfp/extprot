open Ptypes

type tag = int

type low_level =
    Vint of vint_meaning * type_options
  | Bitstring32 of type_options
  | Bitstring64 of b64_meaning * type_options
  | Bytes of type_options
  | Sum of [`Constant of constructor | `Non_constant of constructor * low_level list] list * type_options
  | Record of string * field list * type_options
  | Tuple of low_level list * type_options
  | Htuple of htuple_meaning * low_level * type_options
  | Message of string list * string * type_options

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
  | Message_single of string option * (string * bool * 'a) list
    (* namespace (for poly record types) * list of  constructor * mutable * type *)
  | Message_sum of (string option * string * (string * bool * 'a) list) list
    (* list of  namespace * constructor * list of fields as above *)
  | Message_alias of string list * string (* path * name *)

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
  | `Ext_message of string list * string * type_options
]

type poly_type_expr_core = [
    poly_type_expr_core base_type_expr_core
  | `Type of string * type_param list * poly_type_expr_core list * type_options (* polymorphic sum type name, type args *)
  | `Ext_type of string list * string * poly_type_expr_core list * type_options
  | `Type_arg of string
]

type poly_type_expr = [
  poly_type_expr_core
  | `Record of poly_type_expr_core record_data_type * type_options
  | `Sum of poly_type_expr_core sum_data_type * type_options
]

let reduced_type_expr e = (e :> reduced_type_expr)

type bindings = declaration SMap.t
