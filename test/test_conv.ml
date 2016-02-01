open OUnit
open Test_types
open Test_util
module PP = Extprot.Pretty_print
module M = Extprot.Msg_buffer
module C = Extprot.Conv

let aeq pp exp actual = assert_equal ~printer:(PP.pp pp) exp actual

let check_roundtrip pp enc dec v1 v2 =
  let enc = encode enc v1 in
    aeq pp v2 (decode dec enc)

let raises_extprot_err ?msg (f : 'a -> unit) x =
  try
    f x;
    match msg with
        None -> assert_failure "Extprot_error expected."
      | Some m -> assert_failure ("Extprot_error expected: " ^ m)
  with Extprot.Error.Extprot_error _ -> ()

let test_serialize_versioned_roundtrip v =
  let s = C.serialize_versioned [| Simple_tuple.write |] 0 v in
  let v' = C.deserialize_versioned [| Simple_tuple.read |] s in
    aeq Simple_tuple.pp v v'

let pputs fmt = Format.printf (fmt ^^ "@.")

let test_read_frame_deserialize' v =
  let s = C.serialize_versioned [| Simple_tuple.write |] 0 v in
  let version, msg = C.read_frame (IO.input_string s) in
  let v' = C.deserialize_versioned' [| Simple_tuple.read |] version msg in
    aeq Simple_tuple.pp v v'

let values = [ { Simple_tuple.v = (42, true) } ]

let with_values f () = List.iter f values

let () = Register_test.register "Conv"
  [
    "Serialize versioned roundtrip" >::
      with_values test_serialize_versioned_roundtrip;
    "read_frame + deserialize_versioned'" >::
      with_values test_read_frame_deserialize';
  ]
