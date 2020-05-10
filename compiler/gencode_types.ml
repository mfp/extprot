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
  | Message of string list * string * low_level message option * type_options

and constructor = {
  const_tag : tag;
  const_name : string;
  const_type : string;
}

and field = {
  field_name : string;
  field_type : low_level;
  field_lazy : bool;
}

and 'a message =
  | Message_single of namespace option * (field_name * field_mutable * ev_regime * 'a) list
  | Message_sum of (namespace option * constructor_name * (field_name * field_mutable * ev_regime * 'a) list) list
  | Message_alias of string list * string (* path * name *)
  | Message_subset of msg_name * (field_name * field_mutable * ev_regime * 'a) list * 'a field_subset

and namespace        = string
and constructor_name = string
and msg_name         = string
and field_name       = string
and field_mutable    = bool
and ev_regime        = [ `Eager | `Lazy ]

and 'a field_subset =
  | Include_fields of (string * 'a option) list
  | Exclude_fields of string list

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

let kind_of_reduced_type_expr : reduced_type_expr -> string = function
| #base_type_expr_core as x -> kind_of_base_type_expr_core x
| `Sum _ -> "union"
| `Record _ -> "record"
| `Message _ -> "message"
| `Ext_message _ -> "external message"

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
