open Printf
open ExtList

type type_options = (string * string) list

let dump_type_options l =
  let pr_list name l =
    printf "%s options:\n" name;
    List.iter (fun (k, v) -> printf "%s.%S = %S\n" name k v) l in
  let dump_opt = function
      `Global l -> pr_list "Global" l
    | `OCaml l -> pr_list "OCaml" l
  in List.iter dump_opt l

type base_type_expr_simple = [
    `Bool of type_options
  | `Byte of type_options
  | `Int of type_options
  | `Long_int of type_options
  | `Float of type_options
  | `String of type_options
]

let kind_of_base_type_expr_simple = function
| `Bool _ -> "bool"
| `Byte _ -> "byte"
| `Int _ -> "int"
| `Long_int _ -> "long"
| `Float _ -> "float"
| `String _ -> "string"

type 'a base_type_expr_core = [
    base_type_expr_simple
  | `Tuple of 'a list * type_options
  | `List of 'a * type_options
  | `Array of 'a * type_options
]

let kind_of_base_type_expr_core = function
| #base_type_expr_simple as x -> kind_of_base_type_expr_simple x
| `Tuple _ -> "tuple"
| `List _ -> "list"
| `Array _ -> "array"

module Type_param : sig
  type type_param
  val type_param_of_string : string -> type_param
  val string_of_type_param : type_param -> string
  val type_param_name : type_param -> string
end =
struct
  type type_param = string
  let type_param_of_string x = x
  let string_of_type_param x = "'" ^ x
  let type_param_name x = x
end

include Type_param

type base_type_expr = [
    base_type_expr base_type_expr_core
  | `App of string * base_type_expr list * type_options
  | `Ext_app of string list * string * base_type_expr list * type_options
  | `Type_param of type_param
]

let kind_of_base_type_expr = function
| #base_type_expr_core as x -> kind_of_base_type_expr_core x
| `App _ -> "type application"
| `Ext_app _ -> "external type application"
| `Type_param _ -> "type parameter"

type type_expr = [
    base_type_expr
  | `Record of base_type_expr record_data_type * type_options
  | `Sum of base_type_expr sum_data_type * type_options
]

and 'a record_data_type = {
  record_name : string;
  record_fields : (string * bool * [`Lazy | `Eager | `Auto] * field_options * 'a) list
}

and field_options = (string * string) list

and 'a sum_data_type = {
  type_name : string;
  constructors : [`Constant of string | `Non_constant of string * 'a list] list
}

let kind_of_type_expr = function
| #base_type_expr as x -> kind_of_base_type_expr x
| `Record -> "record"
| `Sum -> "union"

type base_message_expr = [ `Message_record of (string * bool * eval_regime * field_options * base_type_expr) list ]
and eval_regime = [`Eager | `Lazy | `Auto]

type message_expr_app = [ `Message_app of string * base_type_expr list * type_options ]

type message_expr = [
    base_message_expr
  | message_expr_app
  | `Message_alias of string list * string
  | `Message_sum of (string * [base_message_expr | message_expr_app]) list
  | `Message_subset of string * (string * (base_type_expr option * eval_regime option)) list * [`Include | `Exclude]
]

let kind_of_message_expr = function
| `Message_record _ -> "record"
| `Message_app _ -> "application"
| `Message_alias -> "message alias"
| `Message_sum -> "union"
| `Message_subset -> "subset"

type declaration =
    Message_decl of string * message_expr * export * type_options
  | Type_decl of string * type_param list * type_expr * type_options

and export = Export_YES | Export_NO

type origin = Extern | Local

type toplevel =
  | Decl of declaration
  | Include of string

let base_type_expr e = (e :> base_type_expr)
let type_expr e = (e :> type_expr)

let constant_constructors sum =
  List.filter_map
    (function `Constant x -> Some x | `Non_constant _ -> None)
    sum.constructors

let non_constant_constructors sum =
  List.filter_map
    (function `Constant _ -> None | `Non_constant (n, l) -> Some (n, l))
    sum.constructors
