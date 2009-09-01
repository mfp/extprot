
open OUnit
open Printf
module E = Extprot
module PP = E.Pretty_print
open Test_types

let check_write ?msg expected f v () =
  assert_equal ?msg ~printer:(sprintf "%S") expected (E.Conv.serialize f v)

let bits n shift =
  Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n shift) 0xFFL))

let check_bits64 ~prefix check v n : unit =
  check v
    (sprintf "\001\010\001%c%c%c%c%c%c%c%c%c"
       (Char.chr prefix)
       (bits n 0)  (bits n 8)  (bits n 16) (bits n 24)
       (bits n 32) (bits n 40) (bits n 48) (bits n 56))

let (@@) f x = f x

let wrap_printer f x = String.concat "\n" [""; f x; ""]

module Versioned =
struct
  module C = Extprot.Conv

  let (@.) f g x = f (g x)
  let (@..) f g x y = f x (g y)

  let fail () = assert false

  let assert_raise ~msg choose_exn f x =
    try
      let () = f x in
        assert_failure ("Expected exception. " ^ msg)
    with exn ->
      if not (choose_exn exn) then raise exn

  let test_aux
        rd_bool rd_string rd_long serialize_versioned deserialize_versioned =

    let wr_bool, wr_string, wr_long =
      Simple_bool.write_simple_bool,
      Simple_string.write_simple_string,
      Simple_long.write_simple_long in
    let fs1 = [| (fun x -> `Bool x) @. rd_bool;
                 (fun x -> `String x) @. rd_string |] in

    let fs1' = [|
      wr_bool @.. (function `Bool x -> x | _ -> fail ());
      wr_string @.. (function `String x -> x | _ -> fail ());
    |] in

    let fs2 =
      Array.append fs1 [| (fun x -> `Long x) @. rd_long; |] in

    let fs2' =
      Array.append fs1' [|
        wr_long @.. (function `Long x -> x | _ -> fail ());
      |] in

    let v = `Bool { Simple_bool.v = true } in
    let s = serialize_versioned fs1' 0 v in
    let x = deserialize_versioned fs1 s in
    let () =
      assert_equal ~msg:"Versioned serialization failed for `Bool _ ." v x in
    let v = `String { Simple_string.v = "whatever" } in
    let s = serialize_versioned fs1' 1 v in
    let x = deserialize_versioned fs1 s in
    let () =
      assert_equal ~msg:"Versioned serialization failed for `String _." v x in
    let v = `Long { Simple_long.v = 42L } in
    let s = serialize_versioned fs2' 2 v in
    let x = deserialize_versioned fs2 s in
    let () =
      assert_equal ~msg:"Versioned serialization failed for `Long _." v x
    in
      assert_raise
        ~msg:"when finding an unknown version number during deserialization"
        (function C.Wrong_protocol_version _ -> true | _ -> false)
        (fun () -> ignore (deserialize_versioned fs1 s))
        ();
      assert_raise
        ~msg:"when giving a bad version number for serialization"
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> ignore (serialize_versioned fs2' 10 v))
        ();
      ()

  let test_serialize_versioned () =
    test_aux
      Simple_bool.read_simple_bool
      Simple_string.read_simple_string
      Simple_long.read_simple_long
      C.serialize_versioned C.deserialize_versioned

  let test_read_write_versioned () =

    let serialize fs idx v =
      let io = IO.output_string () in
        C.write_versioned fs idx io v;
        IO.close_out io in

    let deserialize fs s =
      let io = IO.input_string s in
        C.read_versioned fs io
    in
      test_aux
      Simple_bool.io_read_simple_bool
      Simple_string.io_read_simple_string
      Simple_long.io_read_simple_long
        serialize deserialize

  let () = Register_test.register "versioned serialization"
    [
      "versioned (de)serialize" >:: test_serialize_versioned;
      "versioned read/write" >:: test_read_write_versioned;
    ]
end

module Probabilistic =
struct
  open Extprot.Random_gen
  open Util

  let check_roundtrip write read prettyprint v =
    (* print_endline @@ prettyprint v; *)
    let enc = encode write v in
      (* print_endline @@ PP.pp (E.Inspect_msg.inspect ~verbose:false) (IO.input_string enc); *)
      try
        assert_equal ~printer:(wrap_printer prettyprint) v (decode read enc)
      with E.Error.Extprot_error (err, loc) ->
        assert_failure @@
        sprintf "%s\nfor\n %s\nencoded as\n%s =\n%s =\n%s\n"
          (PP.pp E.Error.pp_extprot_error (err, loc))
          (prettyprint v)
          (PP.pp PP.pp_dec_bytes enc)
          (PP.pp PP.pp_hex_bytes enc)
          (PP.pp E.Inspect_msg.inspect (IO.input_string enc))

  let iterations = 25000

  let () = Register_test.register "roundtrip"
    [
      "complex type" >:: begin fun () ->
        for i = 0 to 5000 do
          let v = Gen_data.generate Gen_data.complex_rtt in
            check_roundtrip
              Complex_rtt.write_complex_rtt Complex_rtt.read_complex_rtt
              (PP.pp Complex_rtt.pp_complex_rtt) v
        done
      end;

      "message using record type" >:: begin fun () ->
        for i = 0 to 5000 do
          let v = Gen_data.generate Gen_data.rec_message in
            check_roundtrip
              Rec_message.write_rec_message Rec_message.read_rec_message
              (PP.pp Rec_message.pp_rec_message) v
        done
      end;

      "message sum using record types" >:: begin fun () ->
        for i = 0 to 5000 do
          let v = Gen_data.generate Gen_data.rec_message_sum in
            check_roundtrip
              Rec_message_sum.write_rec_message_sum Rec_message_sum.read_rec_message_sum
              (PP.pp Rec_message_sum.pp_rec_message_sum) v
        done
      end;

      "fields of record type" >:: begin fun () ->
        for i = 0 to 5000 do
          let v = Gen_data.generate Gen_data.rec_fields in
            check_roundtrip
              Rec_fields.write_rec_fields Rec_fields.read_rec_fields
              (PP.pp Rec_fields.pp_rec_fields) v
        done
      end;

      "integer" >:: begin fun () ->
        let check n =
          check_roundtrip
            Simple_int.write_simple_int Simple_int.read_simple_int
            (PP.pp Simple_int.pp_simple_int)
            { Simple_int.v = n }
        in
          List.iter check
             [
               0; 1; -1; max_int; min_int; min_int - min_int / 2;
             ];
          for i = 0 to iterations do
            check (Gen_data.generate rand_int)
          done
      end;

      "bool" >:: begin fun () ->
        let check v =
          check_roundtrip
            Simple_bool.write_simple_bool Simple_bool.read_simple_bool
            (PP.pp Simple_bool.pp_simple_bool)
            { Simple_bool.v = v }
        in check true; check false
      end;

      "byte" >:: begin fun () ->
        let check v =
          check_roundtrip
            Simple_byte.write_simple_byte Simple_byte.read_simple_byte
            (PP.pp Simple_byte.pp_simple_byte)
            { Simple_byte.v = v }
        in for i = 0 to 255 do check i done
      end;

      "long" >:: begin fun () ->
        let check v =
          check_roundtrip
            Simple_long.write_simple_long Simple_long.read_simple_long
            (PP.pp Simple_long.pp_simple_long)
            { Simple_long.v = v }
        in for i = 0 to iterations do check (Gen_data.generate rand_int64) done
      end;

      "float" >:: begin fun () ->
        let check v =
          try
            check_roundtrip
              Simple_float.write_simple_float Simple_float.read_simple_float
              (PP.pp Simple_float.pp_simple_float)
              { Simple_float.v = v }
          with e -> match classify_float v with
              FP_nan -> ()
            | _ -> raise e
        in for i = 0 to iterations do check (Gen_data.generate rand_float) done
      end;

      "string" >:: begin fun () ->
        let check v =
          check_roundtrip
            Simple_string.write_simple_string Simple_string.read_simple_string
            (PP.pp Simple_string.pp_simple_string)
            { Simple_string.v = v }
        in for i = 0 to iterations do
          check (Gen_data.generate (rand_string (rand_integer 10)))
        done
      end;

      "abstract type (Digest_type.t)" >:: begin fun () ->
        let check v =
          check_roundtrip
            Simple_digest.write_simple_digest Simple_digest.read_simple_digest
            (PP.pp Simple_digest.pp_simple_digest)
            { Simple_digest.digest = v }
        in for i = 0 to iterations / 10 do
          check (Digest_type.random ())
        done
      end;

      "sum type" >:: begin fun () ->
        let check v =
          check_roundtrip
            Simple_sum.write_simple_sum Simple_sum.read_simple_sum
            (PP.pp Simple_sum.pp_simple_sum)
            { Simple_sum.v = v } in
        let rand_simple_sum =
          Gen_data.rand_sum_type rand_bool (rand_integer 255)
            (rand_string Gen_data.rand_len)
        in
          for i = 0 to iterations do
            check (Gen_data.generate rand_simple_sum)
          done
      end;

      "lists and arrays" >:: begin fun () ->
        let check v =
          check_roundtrip
            Lists_arrays.write_lists_arrays Lists_arrays.read_lists_arrays
            (PP.pp Lists_arrays.pp_lists_arrays)
            v in
        let rand_lists_arrays =
          rand_list Gen_data.rand_len (rand_integer 255) >>= fun lint ->
          rand_array Gen_data.rand_len rand_bool >>= fun abool ->
            return { Lists_arrays.lint = lint; abool = abool }
        in
          for i = 0 to iterations do
            check (Gen_data.generate rand_lists_arrays)
          done
      end;

    ]
end

let () =
  Register_test.register "write composed types"
    [
      "tuple" >::
        (*
         * 001        tuple, tag 0
         * NNN        len
         * 001        elements
         *  001        tuple, tag 0
         *  NNN        len
         *  002        elements
         *   000 020    vint(10)
         *   002 001    bool(true)
         * *)
        check_write "\001\008\001\001\005\002\000\020\002\001"
          ~msg:"{ Simple_tuple.v = (10, true) }"
          Simple_tuple.write_simple_tuple { Simple_tuple.v = (10, true) };

      "msg_sum" >:: begin fun () ->
        (*
         * 001      tuple, tag 0
         * 003      len
         * 001      nelms
         *  002 000  bool false
         * *)
        check_write "\001\003\001\002\000"
          ~msg:"(Msg_sum.A { Msg_sum.b = false })"
          Msg_sum.write_msg_sum (Msg_sum.A { Msg_sum.b = false }) ();
        (*
         * 017      tuple, tag 1
         * 003      len
         * 001      nelms
         *  000 020  vint 10
         * *)
        check_write "\017\003\001\000\020"
          ~msg:"(Msg_sum.B { Msg_sum.i = 10 })"
          Msg_sum.write_msg_sum (Msg_sum.B { Msg_sum.i = 10 }) ()
      end;

      "simple_sum" >:: begin fun () ->
        (*
         * 001       tuple, tag 0
         * 006       len
         * 001       nelms
         *  001       tuple, tag 0
         *  003       len
         *  001       nelms
         *   002 001   bool true
         * *)
        check_write "\001\006\001\001\003\001\002\001"
          ~msg:"{ Simple_sum.v = Sum_type.A true }"
          Simple_sum.write_simple_sum { Simple_sum.v = Sum_type.A true } ();
        (*
         * 001       tuple, tag 0
         * 007       len
         * 001       nelms
         *  017       tuple, tag 1
         *  004       len
         *  001       nelms
         *   002 128  byte 128
         * *)
        check_write "\001\006\001\017\003\001\002\128"
          ~msg:"{ Simple_sum.v = Sum_type.B 128 }"
          Simple_sum.write_simple_sum { Simple_sum.v = Sum_type.B 128 } ();
        (*
         * 001       tuple, tag 0
         * 010       len
         * 001       nelms
         *  033       tuple, tag 2
         *  007       len
         *  001       nelms
         *   003 004 abcd  bytes "abcd"
         * *)
        check_write "\001\010\001\033\007\001\003\004abcd"
          ~msg:"{ Simple_sum.v = Sum_type.C \"abcd\" }"
          Simple_sum.write_simple_sum { Simple_sum.v = Sum_type.C "abcd" } ();
        (*
         * 001       tuple, tag 0
         * 002       len
         * 001       nelms
         *  010       enum, tag 0
         * *)
        check_write "\001\002\001\010"
          ~msg:"{ Simple_sum.v = Sum_type.D }"
          Simple_sum.write_simple_sum { Simple_sum.v = Sum_type.D } ();
      end;

      "nested message" >:: begin fun () ->
        (* 001       tuple, tag 0
         * 015       len
         * 002       nelms
         *  001       tuple, tag 0
         *  010       len
         *  001       nelms
         *   033       tuple, tag 2
         *   007       len
         *   001       nelms
         *    003 004 abcd  bytes "abcd"
         *  000 020   int 10
         * *)
        check_write "\001\015\002\001\010\001\033\007\001\003\004abcd\000\020"
          Nested_message.write_nested_message
          ~msg:"{ Nested_message.v = { Simple_sum.v = Sum_type.C \"abcd\" }; b = 10 }"
          { Nested_message.v = { Simple_sum.v = Sum_type.C "abcd" }; b = 10 }
          ()
      end;

      "lists and arrays" >:: begin fun () ->
        (* 001       tuple, tag 0
         * 018       len
         * 002       nelms
         *  005       htuple, tag 0
         *  006       len
         *  002       nelms
         *   000 020  int(10)
         *   000 128 004 int(256)
         *
         *  005       htuple, tag 0
         *  007       len
         *  003       nlems
         *   002 001   true
         *   002 000   false
         *   002 000   false
         * *)
        check_write
          "\001\018\002\005\006\002\000\020\000\128\004\005\007\003\002\001\002\000\002\000"
          Lists_arrays.write_lists_arrays
          ~msg:"{ Lists_arrays.lint = [10; 256]; abool = [| true; false; false |] }"
          { Lists_arrays.lint = [10; 256]; abool = [| true; false; false |] }
          ()
      end;
    ]

let () =
  Register_test.register "write simple types"
    [
      "bool (true)" >::
        check_write "\001\003\001\002\001"
          Simple_bool.write_simple_bool { Simple_bool.v = true };

      "bool (false)" >::
        check_write "\001\003\001\002\000"
          Simple_bool.write_simple_bool { Simple_bool.v = false };

      "byte" >:: begin fun () ->
        for n = 0 to 255 do
          check_write (sprintf "\001\003\001\002%c" (Char.chr n))
            Simple_byte.write_simple_byte { Simple_byte.v = n } ()
        done;
      end;

      "int" >:: begin fun () ->
        let check n expected =
          check_write ~msg:(sprintf "int %d" n) expected
            Simple_int.write_simple_int { Simple_int.v = n } ()
        in
          check 0 "\001\003\001\000\000";
          for n = 1 to 63 do
            check n (sprintf "\001\003\001\000%c" (Char.chr (2*n)));
            check (-n)
              (sprintf "\001\003\001\000%c" (Char.chr ((2 * lnot (-n)) lor 1)))
          done;
          check 64 "\001\004\001\000\128\001";
          for n = 65 to 8191 do
            check n
              (sprintf "\001\004\001\000%c%c"
                 (Char.chr ((2*n) mod 128 + 128)) (Char.chr ((2*n) / 128)));
            let n' = (2 * lnot (-n)) lor 1 in
              check (-n)
                (sprintf "\001\004\001\000%c%c"
                   (Char.chr (n' mod 128 + 128)) (Char.chr (n' / 128)))
          done
      end;

      "long" >:: begin fun () ->
        let rand_int64 () =
          let upto = match Int64.shift_right_logical (-1L) (8 * Random.int 8) with
              l when l > 0L -> l
            | _ -> Int64.max_int
          in Random.int64 upto in
        let check_long n expected =
          check_write ~msg:(sprintf "long %s" (Int64.to_string n)) expected
            Simple_long.write_simple_long { Simple_long.v = n } ()
        in
          for i = 0 to 10000 do
            let n = rand_int64 () in
              check_bits64 ~prefix:6 check_long n n
          done;
      end;

      "float" >:: begin fun () ->
        let check_float n expected =
          check_write ~msg:(sprintf "float %f" n) expected
            Simple_float.write_simple_float { Simple_float.v = n } ()
        in
          for i = 0 to 1000 do
            let fl = Random.float max_float -. Random.float max_float in
            let n = Int64.bits_of_float fl in
              check_bits64 ~prefix:8 check_float fl n
          done
      end;

      "string" >:: begin fun () ->
        let check_string s expected =
          check_write ~msg:(sprintf "string %S" s) expected
            Simple_string.write_simple_string { Simple_string.v = s } ()
        in
          for len = 0 to 124 do
            let s = String.create len in
              check_string s
                (sprintf "\001%c\001\003%c%s" (Char.chr (len + 3)) (Char.chr len) s)
          done;
          let s = String.create 128 in
            check_string s (sprintf "\001\132\001\001\003\128\001%s" s)
      end;

    ]


let () =
  Register_test.register "error recovery"
    [
      "skip to EOM on error (conversion)" >:: begin fun () ->
        (* we write an extended message with extra fields to make sure they are
         * skipped when there's an error in the conversion function for the
         * original message definition *)
        let digests =
          List.map (fun digest -> { Simple_digest2.digest = digest; extra = "whatever" })
            [ Digest_type.bad_digest; Digest_type.from_string "foo" ] in
        let s = String.concat "" @@
                List.map (Util.encode Simple_digest2.write_simple_digest2) digests in
        let reader = E.Reader.String_reader.make s 0 (String.length s) in
          begin
            try
              ignore (Simple_digest.read_simple_digest reader);
              assert_failure
                "Should raise exception for conversion error in Digest_type.from_string"
            with _ -> ()
          end;
          assert_equal
            ~printer:(wrap_printer (PP.pp Simple_digest.pp_simple_digest))
            { Simple_digest.digest = Digest_type.from_string "foo" }
            (Simple_digest.read_simple_digest reader)
      end
    ]
