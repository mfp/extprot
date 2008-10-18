
open OUnit
open Printf
module E = Extprot
open Test_types

let check_write ?msg expected f v () =
  let b = E.Msg_buffer.create () in
    f b v;
    assert_equal ?msg ~printer:(sprintf "%S") expected (E.Msg_buffer.contents b)

let bits n shift =
  Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n shift) 0xFFL))

let check_bits64 check v n =
  check v
    (sprintf "\001\010\001\004%c%c%c%c%c%c%c%c"
       (bits n 0)  (bits n 8)  (bits n 16) (bits n 24)
       (bits n 32) (bits n 40) (bits n 48) (bits n 56))

let () =
  Register_test.register "write (unit)"
    [
      "bool (true)" >::
        check_write "\001\003\001\000\001"
          Simple_bool.write_simple_bool { Simple_bool.v = true };

      "bool (false)" >::
        check_write "\001\003\001\000\000"
          Simple_bool.write_simple_bool { Simple_bool.v = false };

      "byte" >:: begin fun () ->
        for n = 0 to 127 do
          check_write (sprintf "\001\003\001\000%c" (Char.chr n))
            Simple_byte.write_simple_byte { Simple_byte.v = n } ()
        done;
        for n = 128 to 255 do
          check_write (sprintf "\001\004\001\000%c\001" (Char.chr n))
            Simple_byte.write_simple_byte { Simple_byte.v = n } ()
        done
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

      "unsigned int" >:: begin fun () ->
        let check n expected =
          check_write ~msg:(sprintf "int %d" n) expected
            Simple_unsigned.write_simple_unsigned { Simple_unsigned.v = n } ()
        in
          for n = 0 to 127 do
            check n (sprintf "\001\003\001\000%c" (Char.chr n));
          done;
          for n = 128 to 16383 do
            check n
              (sprintf "\001\004\001\000%c%c"
                 (Char.chr (n mod 128 + 128)) (Char.chr (n / 128)));
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
              check_bits64 check_long n n
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
              check_bits64 check_float fl n
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
