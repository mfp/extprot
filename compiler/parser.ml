open Camlp4.PreCast
open Ptypes

(* use fresh module *)
module Gram = MakeGram(Lexer)

let entries = Gram.Entry.mk "type_expr"

type constructor =
  [ `Constant of string
  | `Non_constant of string * base_type_expr list ]

let sum_of_constructor_list l =
  {
    type_name = "bogus";
    constructors = l;
  }

let make_complex_msg_expr n = function
    `Alias (p, name)  -> `Message_alias (n :: p, name)
  | `Sum (x, l) -> `Message_sum ((n, x) :: l)

EXTEND Gram
  GLOBAL: entries;

  entries :
    [
      [ m = entry; l = entries -> (m :: l)
      | `EOI -> [] ] ];

  entry :
    [ "message"
        [ "message"; name = a_LIDENT; "="; e = msg_expr; opts = type_options ->
            `Decl (Message_decl (name, e, opts)) ]
    | "include"
        [ "include"; file = a_STRING -> `Include file ]
    | "type"
        [ "type"; name = a_LIDENT;
          par = LIST0 [ "'"; n = a_LIDENT -> type_param_of_string n];
          "="; e = type_expr; opts = type_options ->
            let e = match e with
                `Sum (s, opts) -> `Sum ({ s with type_name = name }, opts)
              | `Record (r, opts) -> `Record ({ r with record_name = name }, opts)
              | e -> e
            in `Decl (Type_decl (name, par, e, opts)) ] ];

  type_options :
    [ [ -> []
      | "options"; l = LIST0 [type_option_values] -> l ] ];

  type_option_values :
    [ [ name = a_STRING; "="; value = a_STRING -> (name, value) ] ];

  type_expr :
    [ "top"
      [ l = LIST1 [ const_declarations ] SEP "|" -> `Sum (sum_of_constructor_list l, [])
      | r = record_type -> r
      ]
    | "simple"
      [ t = type_expr_simple -> (t : base_type_expr :> type_expr)] ] ;

  const_declarations :
    [ [ n = a_UIDENT; t = const_params -> `Non_constant (n, t)
      | n = a_UIDENT -> `Constant n ] ];

  const_params :
    [ [ l = LIST1 [ type_expr_simple ] -> l ] ];

  type_expr_simple :
    [ "top"

        [ "["; t = SELF; "]" -> `List (t, [])
        | "[|"; t = SELF; "|]" -> `Array (t, [])
        | tup = tuple -> `Tuple tup
        | n = a_LIDENT; "<"; targs = LIST1 [ type_expr_simple ] SEP ","; ">" ->
            `App (n, targs, [])
        | "'"; n = a_LIDENT -> `Type_param (type_param_of_string n)
        | n = a_LIDENT -> `App (n, [], [])
        | n = ident_with_path -> let (path, name) = n in `Ext_app (path, name, [], [])
        ]

    | "simple"
        [ "bool" -> `Bool []
        | "byte" -> `Byte []
        | "int" -> `Int []
        | "long" -> `Long_int []
        | "float" -> `Float []
        | "string" -> `String [] ] ] ;

  ident_with_path :
    [
      [ p = a_UIDENT; "."; n = a_LIDENT -> ([p], n)
      | p = a_UIDENT; "."; p1 = SELF -> let (p1, n) = p1 in (p :: p1, n) ] ];

  tuple :
    [ [ "("; l = LIST1 [ type_expr_simple ] SEP "*"; ")" -> (l, [])] ] ;

  record_type :
    [ [ "{"; l = field_list; "}" ->
          `Record ({ record_name = "bogus"; record_fields = l }, []) ] ];

  field_list :
    [ [ t1 = field; ";"; t2 = SELF -> t1 :: t2
      | t1 = field; ";" -> [t1]
      | t1 = field -> [t1] ] ];

  field :
    [ [ n = a_LIDENT; ":"; ty = type_expr_simple -> (n, false, ty)
      | "mutable"; n = a_LIDENT; ":"; ty = type_expr_simple -> (n, true, ty) ] ];

  record_app :
    [ [ n = a_LIDENT; "<"; targs = LIST1 [ type_expr_simple ] SEP ","; ">" ->
        `Message_app (n, targs, [])
      | n = a_LIDENT -> `Message_app (n, [], [])
      ] ];

  record :
    [ [ "{"; l = field_list; "}" -> `Message_record l
      ] ];

  record_or_app :
    [ [ r = record -> r
      | r = record_app -> r ] ];

  msg_expr :
    [ [ r = record_or_app -> (r :> message_expr)
      | n = a_UIDENT; x = complex_msg_expr -> make_complex_msg_expr n x
      | "{|"; n = a_LIDENT; "with"; l = LIST1 [ a_LIDENT ] SEP ";"; "|}" ->
        `Message_subset (n, l, `Include)
      | "{|"; n = a_LIDENT; "not"; l = LIST1 [ a_LIDENT ] SEP ";"; "|}" ->
        `Message_subset (n, l, `Exclude)
      ] ];

  complex_msg_expr:
    [ [ "."; l = LIST0 [ n = a_UIDENT -> n ] SEP "."; name = a_LIDENT ->
          `Alias (l, name)
      | x = record_or_app -> `Sum (x, [])
      | x = record_or_app; "|"; l = LIST1 [ n = a_UIDENT; r = record_or_app -> (n, r) ] SEP "|" ->
          `Sum (x, l) ] ];

  a_LIDENT: [ [ `LIDENT s -> s ] ];
  a_UIDENT: [ [ `UIDENT s -> s ] ];
  a_STRING: [ [ `STRING (s, _) -> s ] ];

END

(* let parse text = Gram.parse_string entries (Loc.mk "<string>") text *)
let rec parse_file fname =
  let l = Gram.parse_string entries (Loc.mk fname) (Std.input_file fname) in
  let l = List.map begin function
    | `Decl decl -> [Decl decl,Local]
    | `Include file ->
      let included = List.map (fun (x,_) -> x, match x with Decl _ -> Extern | Include _ -> Local) (parse_file file) in
      included @ [ Include file, Local ]
    end l
  in
  List.flatten l

let print_synerr f x =
  try
    f x
  with Loc.Exc_located (loc, e) -> print_endline (Loc.to_string loc); raise e
