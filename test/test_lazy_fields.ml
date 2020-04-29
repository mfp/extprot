open Printf
open OUnit
open Test_types
open Test_util
module PP = Extprot.Pretty_print
module M  = Extprot.Msg_buffer
module F  = Extprot.Field

let aeq pp exp actual = assert_equal ~printer:(PP.pp pp) exp actual

let check_write ?msg expected f v () =
  assert_equal ?msg ~printer:(sprintf "%S") expected (E.Conv.serialize f v)

let check_roundtrip force pp enc dec v =
  aeq pp (force v) (force (decode dec (encode enc v)))

let check_roundtrip_complex force pp enc dec v1 v2 =
  aeq pp (force v2) (force (decode dec (encode enc v1)))

let raises_extprot_err ?msg (f : 'a -> unit) x =
  try
    f x;
    match msg with
        None -> assert_failure "Extprot_error expected."
      | Some m -> assert_failure ("Extprot_error expected: " ^ m)
  with Extprot.Error.Extprot_error _ -> ()

let thunk x =
  F.from_thunk (fun () -> x)

let force_lazy01 t =
  ignore (F.force t.Lazy01.a);
  ignore (F.force t.Lazy01.b);
  ignore (F.force t.Lazy01.c);
  ignore (F.force t.Lazy01.d);
  ignore (F.force t.Lazy01.e);
  ignore (F.force t.Lazy01.f);
  t

let force_lazy02 t =
  ignore (F.force t.LazyT.v);
  t

let force_lazy03 t =
  ignore (F.force t.Lazy03.v);
  ignore (F.force t.Lazy03.v2);
  t

let force_lazyT t =
  ignore (F.force t.LazyT.v);
  t

let tests = "lazy fields" >::: [

  "write primitive types (values)" >:: begin fun () ->
    (*
     * 01        tuple, tag 0
     * NNN        len
     * 12        elements 6
     *  02 01   bool true
     *  02 42   byte 42
     *  00 26   vint 13
     *  06 42 00 00 00 00 00 00 00   long 42
     *  08 00 00 00 00 00 00 45 40   float 42.
     *  03 04 97 98 99 46            bytes "abc."
     * *)
    check_write
      "\001\031\006\
       \002\001\
       \002\042\
       \000\026\
       \006\042\000\000\000\000\000\000\000\
       \008\000\000\000\000\000\000\069\064\
       \003\004\097\098\099\046"
      ~msg:"{ Lazy01.a = true; b = 42; c = 13; d = 42L; e = 42.; f = \"abc.\" }"
      Lazy01.write
      { Lazy01.
        a = F.from_val true;
        b = F.from_val 42;
        c = F.from_val 13;
        d = F.from_val 42L;
        e = F.from_val 42.;
        f = F.from_val "abc.";
      }
      ();
  end;

  "write primitive types (thunks)" >:: begin fun () ->
    (*
     * 01        tuple, tag 0
     * NNN        len
     * 12        elements 6
     *  02 01   bool true
     *  02 42   byte 42
     *  00 26   vint 13
     *  06 42 00 00 00 00 00 00 00   long 42
     *  08 00 00 00 00 00 00 45 40   float 42.
     *  03 04 97 98 99 46            bytes "abc."
     * *)
    check_write
      "\001\031\006\
       \002\001\
       \002\042\
       \000\026\
       \006\042\000\000\000\000\000\000\000\
       \008\000\000\000\000\000\000\069\064\
       \003\004\097\098\099\046"
      ~msg:"{ Lazy01.a = true; b = 42; c = 13; d = 42L; e = 42.; f = \"abc.\" }"
      Lazy01.write
      { Lazy01.
        a = thunk true;
        b = thunk 42;
        c = thunk 13;
        d = thunk 42L;
        e = thunk 42.;
        f = thunk "abc.";
      }
      ();
  end;

  "primitive types" >:: begin fun () ->
    check_roundtrip
      force_lazy01
      Lazy01.pp Lazy01.write Lazy01.read
      { Lazy01.
        a = F.from_val true;
        b = F.from_val 42;
        c = F.from_val (-4242);
        d = F.from_val (-424242L);
        e = F.from_val 42.;
        f = F.from_val "abc.";
      };

    check_roundtrip
      force_lazy01
      Lazy01.pp Lazy01.write Lazy01.read
      { Lazy01.
        a = thunk true;
        b = thunk 42;
        c = thunk (-4242);
        d = thunk (-424242L);
        e = thunk 42.;
        f = thunk "jkldfghdfoghdfgjkh";
      };
  end;

  "record types" >:: begin fun () ->
    check_roundtrip
      force_lazy02
      Lazy02.pp Lazy02.write Lazy02.read
      { LazyT.v = F.from_val 42 };

    check_roundtrip
      force_lazy02
      Lazy02.pp Lazy02.write Lazy02.read
      { LazyT.v = thunk 42 };
  end;

  "lazy with default" >:: begin fun () ->
    check_roundtrip_complex
      force_lazy03
      Lazy03.pp Lazy03b.write Lazy03.read
      { Lazy03b.v = F.from_val 424242 }
      { Lazy03.v = F.from_val 424242; v2 = F.from_val 42 };
  end;
]

let () = Register_test.register "lazy fields"
  [
    tests
  ]
