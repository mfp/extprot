module E = Extprot
module PP = E.Pretty_print
open Printf
open OUnit

let (@@) f x = f x

let encode f v =
  let b = E.Msg_buffer.create () in
    f b v;
    E.Msg_buffer.contents b

let decode f s = f @@ E.Reader.String_reader.make s 0 (String.length s)

let unwrap_extprot_error f x =
  try f x
  with E.Error.Extprot_error (err, loc) ->
    assert_failure @@
    sprintf "Extprot_error (%s)" @@ PP.pp E.Error.pp_extprot_error (err, loc)
