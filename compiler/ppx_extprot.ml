
(* ocamlfind ocamlopt -o ppx_extprot -package ppxlib,ppxlib.metaquot,compiler-libs ppx_extprot.ml -linkpkg *)

open Ppxlib

let source =
{code|

module Foo =
struct
  type foo = { a : int; b : bar }
  let read b = failwith "boom"
  let write b x = failwith "boom"
end

|code}

let expand_function ~loc ~path _ =
  List.hd @@ Parse.implementation @@ Lexing.from_string source

let extension =
  Ppxlib.Extension.declare
    "extprot"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(pstr __)
    expand_function

let rule = Ppxlib.Context_free.Rule.extension extension

let () = Ppxlib.Driver.register_transformation ~rules:[rule] "extprot"

let () = Ppxlib.Driver.standalone ()
