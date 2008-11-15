module E = Extprot
module PP = E.Pretty_print
open Printf
open OUnit

let (@@) f x = f x

let encode = E.Conv.serialize
let decode = E.Conv.deserialize

let unwrap_extprot_error f x =
  try f x
  with E.Error.Extprot_error (err, loc) ->
    assert_failure @@
    sprintf "Extprot_error (%s)" @@ PP.pp E.Error.pp_extprot_error (err, loc)
