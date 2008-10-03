open Camlp4.PreCast
open Printf
open Syntax
open Ptypes
open ExtList

let declarations = Gram.Entry.mk "type_expr"

type constructor =
    Constant of string
  | Non_constant of string * base_type_expr list

let sum_of_constructor_list l =
  {
    constant = List.filter_map (function Constant s -> Some s | _ -> None) l;
    non_constant =
      List.filter_map (function Non_constant (s, l) -> Some (s, l) | _ -> None) l;
  }

EXTEND Gram
  GLOBAL: declarations;

  declarations :
    [
      [ m = declaration; l = declarations -> (m :: l)
      | `EOI -> [] ] ];

  declaration :
    [ "message"
        [ "message"; name = a_LIDENT; "="; e = msg_expr -> Message_decl (name, e) ]
    | "type"
        [ "type"; name = a_LIDENT; par = LIST0 [ a_LIDENT ];
          "="; e = type_expr -> Type_decl (name, par, e) ] ];

  type_expr :
    [ "top"
      [ l = LIST1 [ const_declarations ] SEP "|" -> `Sum (sum_of_constructor_list l)
      (* | r = record -> r  *)
      ]
    | "simple"
      [ t = type_expr_simple -> (t : base_type_expr :> type_expr) ] ];

  const_declarations :
    [ [ n = a_UIDENT; t = const_params -> Non_constant (n, t)
      | n = a_UIDENT -> Constant n ] ];

  const_params :
    [ [ l = LIST1 [ type_expr_simple ] -> l ] ];

  type_expr_simple :
    [ "top"

        [ "["; t = SELF; "]" -> `List t
        | "[|"; t = SELF; "|]" -> `Array t
        | tup = tuple -> `Tuple tup
        | n = a_LIDENT; "<"; targs = LIST1 [ type_expr_simple ] SEP ","; ">" ->
            `App (n, targs)
        | n = a_LIDENT -> `App (n, [])
        ]

    | "simple"
        [ "bool" -> `Bool
        | "byte" -> `Byte
        | "int" -> `Int false;
        | "unsigned"; "int" -> `Int true
        | "long" -> `Long_int
        | "float" -> `Float
        | "string" -> `String ] ];

  tuple :
    [ [ "("; l = LIST1 [ type_expr_simple ] SEP "*"; ")" -> l ] ];

  record :
    [ [ "{"; l = LIST1 [ field ] SEP ";"; "}" -> `Record l ] ];

  field :
    [ [ n = a_LIDENT; ":"; ty = type_expr_simple -> (n, false, ty)
      | "mutable"; n = a_LIDENT; ":"; ty = type_expr_simple -> (n, true, ty) ] ];

  msg_expr :
    [ [ r = record -> (r :> message_expr)
      | l = LIST1 [ n = a_UIDENT; r = record -> (n, r) ] SEP "|" -> `Sum l ] ];

  a_LIDENT: [ [ `LIDENT s -> s ] ];
  a_UIDENT: [ [ `UIDENT s -> s ] ];

END

let input_file fname =
  let b = Buffer.create 4096 in
  let buf = String.create 4096 in
  let rec loop ic = match input ic buf 0 4096 with
      0 -> close_in ic; Buffer.contents b
    | n -> Buffer.add_substring b buf 0 n; loop ic
  in loop (open_in fname)

let parse text = Gram.parse_string declarations (Loc.mk "<string>") text
let parse_file fname =
  Gram.parse_string declarations (Loc.mk fname) (input_file fname)

let print_synerr f x =
  try
    f x
  with Loc.Exc_located (loc, e) -> print_endline (Loc.to_string loc); raise e
