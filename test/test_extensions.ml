open Printf
open OUnit
open Test_types
open Util
module PP = Extprot.Pretty_print
module M = Extprot.Msg_buffer

let aeq pp exp actual = assert_equal ~printer:(PP.pp pp) exp actual

let check_roundtrip pp enc dec v1 v2 =
  let enc = encode enc v1 in
    aeq pp v2 (decode dec enc)

let raises_extprot_err f x =
  try
    f x;
    assert_failure "Extprot_error expected."
  with Extprot.Error.Extprot_error _ -> ()

let () = Register_test.register "extensions"
  [
    "default msg constructor" >:: begin fun () ->
      check_roundtrip Msg1.pp_msg1 Msg1a.write_msg1a Msg1.read_msg1
        (Msg1a.A { Msg1a.a = 123 }) { Msg1.a = 123 };

      raises_extprot_err
        (check_roundtrip Msg1.pp_msg1 Msg1a.write_msg1a Msg1.read_msg1
           (Msg1a.B { Msg1a.b = "123" }))
        { Msg1.a = 123 }
    end;

    "dropping unknown fields" >:: begin fun () ->
      check_roundtrip Msg1a.pp_msg1a Msg1b.write_msg1b Msg1a.read_msg1a
        (Msg1b.A { Msg1b.a = 123; Msg1b.a' = 42; }) (Msg1a.A { Msg1a.a = 123 });
    end;

    "dropping unknown fields (sequence)" >:: begin fun () ->
      let a = Array.init 100 (fun _ -> Random.int 0x3ffffff) in
      let b = M.create () in
        Array.iter
          (fun i -> Msg1b.write_msg1b b (Msg1b.A { Msg1b.a = i; Msg1b.a' = -i; }))
          a;
        let s = M.contents b in
        let io = Extprot.Reader.String_reader.make s 0 (String.length s) in
          Array.iter
            (fun n -> match Msg1a.read_msg1a io with
                 Msg1a.B _ -> assert_failure "Should get Msg1a.A _."
               | Msg1a.A t -> assert_equal ~printer:string_of_int n t.Msg1a.a)
            a
    end;
  ]


