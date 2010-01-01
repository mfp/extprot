open Printf

TYPE_CONV_PATH "Extprot_compiler.Protocol_types"

type type_options = (string * string) list with sexp

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
] with sexp

type 'a base_type_expr_core = [
    base_type_expr_simple
  | `Tuple of 'a list * type_options
  | `List of 'a * type_options
  | `Array of 'a * type_options
] with sexp

module Type_param : sig
  type type_param
  val type_param_of_string : string -> type_param
  val string_of_type_param : type_param -> string
  val type_param_name : type_param -> string
  val type_param_of_sexp : Sexplib.Sexp.t -> type_param
  val sexp_of_type_param : type_param -> Sexplib.Sexp.t
end =
struct
  type type_param = string with sexp
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
] with sexp

type type_expr = [
    base_type_expr
  | `Record of base_type_expr record_data_type * type_options
  | `Sum of base_type_expr sum_data_type * type_options
]

and 'a record_data_type = {
  record_name : string;
  record_fields : (string * bool * 'a) list
}

and 'a sum_data_type = {
  type_name : string;
  constant : string list;
  non_constant : (string * 'a list) list
} with sexp

type base_message_expr = [ `Record of (string * bool * base_type_expr) list ] with sexp

type message_expr_app = [ `App of string * base_type_expr list * type_options ] with sexp

type message_expr = [
    base_message_expr
  | message_expr_app
  | `Sum of (string * [base_message_expr | message_expr_app]) list
] with sexp

type declaration =
    Message_decl of string * message_expr * type_options
  | Type_decl of string * type_param list * type_expr * type_options
with sexp

let base_type_expr e = (e :> base_type_expr)
let type_expr e = (e :> type_expr)
