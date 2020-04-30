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

let check_serialization_equiv ?msg enc1 enc2 v1 v2 =
  assert_equal ?msg ~printer:(sprintf "%S") (encode enc1 v1) (encode enc2 v2)

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
  F.discard_packed t.LazyT.v;
  t

let force_lazy06 t =
  ignore (F.force t.Lazy06.v);
  F.discard_packed t.Lazy06.v;
  t

let force_lazy07 t =
  ignore (F.force t.Lazy07.v);
  F.discard_packed t.Lazy07.v;
  t

let force_lazy10 t =
  ignore (F.force t.Lazy10.v);
  F.discard_packed t.Lazy10.v;
  t

let nop x = x

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

  "write list types (values)" >:: begin fun () ->
     (* 001       tuple, tag 0
      * 009       len
      * 001       nelms
      *  005       htuple, tag 0
      *  006       len
      *  002       nelms
      *   000 020  int(10)
      *   000 128 004 int(256)
      * *)
     check_write
       "\001\
        \009\
        \001\
        \005\006\002\
        \000\020\
        \000\128\004"
       Lazy04.write
       ~msg:"{ LazyT.v = [10; 256]; }"
       { LazyT.v = F.from_val [10; 256]; }
       ()
  end;

  "write list types (thunks)" >:: begin fun () ->
     (* 001       tuple, tag 0
      * 009       len
      * 001       nelms
      *  005       htuple, tag 0
      *  006       len
      *  002       nelms
      *   000 020  int(10)
      *   000 128 004 int(256)
      * *)
     check_write
       "\001\
        \009\
        \001\
        \005\006\002\
        \000\020\
        \000\128\004"
       Lazy04.write
       ~msg:"{ LazyT.v = [10; 256]; }"
       { LazyT.v = thunk [10; 256]; }
       ()
  end;

  "write array types (values)" >:: begin fun () ->
     (* 001       tuple, tag 0
      * 009       len
      * 001       nelms
      *  005       htuple, tag 0
      *  006       len
      *  002       nelms
      *   000 020  int(10)
      *   000 128 004 int(256)
      * *)
     check_write
       "\001\
        \009\
        \001\
        \005\006\002\
        \000\020\
        \000\128\004"
       Lazy05.write
       ~msg:"{ LazyT.v = [|10; 256|]; }"
       { LazyT.v = F.from_val [|10; 256|]; }
       ()
  end;

  "write array types (thunks)" >:: begin fun () ->
     (* 001       tuple, tag 0
      * 009       len
      * 001       nelms
      *  005       htuple, tag 0
      *  006       len
      *  002       nelms
      *   000 020  int(10)
      *   000 128 004 int(256)
      * *)
     check_write
       "\001\
        \009\
        \001\
        \005\006\002\
        \000\020\
        \000\128\004"
       Lazy05.write
       ~msg:"{ LazyT.v = [|10; 256|]; }"
       { LazyT.v = thunk [|10; 256|]; }
       ()
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

  "primitive types, bit-for-bit compat with eager" >:: begin fun () ->
    check_serialization_equiv
      Lazy01.write Lazy01b.write
      { Lazy01.
        a = F.from_val true;
        b = F.from_val 42;
        c = F.from_val (-4242);
        d = F.from_val (-424242L);
        e = F.from_val 42.;
        f = F.from_val "abc.";
      }
      { Lazy01b.
        a = true;
        b = 42;
        c = (-4242);
        d = (-424242L);
        e = 42.;
        f = "abc.";
      };

    check_serialization_equiv
      Lazy01.write Lazy01b.write
      { Lazy01.
        a = thunk true;
        b = thunk 42;
        c = thunk (-4242);
        d = thunk (-424242L);
        e = thunk 42.;
        f = thunk "abc.";
      }
      { Lazy01b.
        a = true;
        b = 42;
        c = (-4242);
        d = (-424242L);
        e = 42.;
        f = "abc.";
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

  "lists" >:: begin fun () ->
    check_roundtrip
      force_lazyT
      Lazy04.pp Lazy04.write Lazy04.read
      { LazyT.v = F.from_val [1; 2; 42] };

    check_roundtrip
      force_lazyT
      Lazy04.pp Lazy04.write Lazy04.read
      { LazyT.v = thunk [1; 2; 42] };

    check_roundtrip
      force_lazyT
      Lazy04.pp Lazy04.write Lazy04.read
      { LazyT.v = F.from_val [] };

    check_roundtrip
      force_lazyT
      Lazy04.pp Lazy04.write Lazy04.read
      { LazyT.v = thunk [] };
  end;

  "arrays" >:: begin fun () ->
    check_roundtrip
      force_lazyT
      Lazy05.pp Lazy05.write Lazy05.read
      { LazyT.v = F.from_val [|1; 2; 42|] };

    check_roundtrip
      force_lazyT
      Lazy05.pp Lazy05.write Lazy05.read
      { LazyT.v = thunk [|1; 2; 42|] };

    check_roundtrip
      force_lazyT
      Lazy05.pp Lazy05.write Lazy05.read
      { LazyT.v = F.from_val [||] };

    check_roundtrip
      force_lazyT
      Lazy05.pp Lazy05.write Lazy05.read
      { LazyT.v = thunk [||] };
  end;

  "tuples" >:: begin fun () ->
    check_roundtrip
      force_lazy07
      Lazy07.pp Lazy07.write Lazy07.read
      { Lazy07.x = 42; v = F.from_val (13, 42.) };

    check_roundtrip
      force_lazy07
      Lazy07.pp Lazy07.write Lazy07.read
      { Lazy07.x = 42; v = thunk (13, 42.) };
  end;

  "tuples, compat with eager fields" >:: begin fun () ->
    check_roundtrip_complex
      force_lazy07
      Lazy07.pp Lazy07b.write Lazy07.read
      { Lazy07b.x = 42; v = (13, 42.) }
      { Lazy07.x = 42; v = F.from_val (13, 42.) };

    check_roundtrip_complex
      nop
      Lazy07b.pp Lazy07.write Lazy07b.read
      { Lazy07.x = 42; v = F.from_val (13, 42.) }
      { Lazy07b.x = 42; v = (13, 42.) };

    check_roundtrip_complex
      nop
      Lazy07b.pp Lazy07.write Lazy07b.read
      { Lazy07.x = 42; v = thunk (13, 42.) }
      { Lazy07b.x = 42; v = (13, 42.) };
  end;

  "sum types" >:: begin fun () ->
    check_roundtrip
      force_lazy06
      Lazy06.pp Lazy06.write Lazy06.read
      { Lazy06.x = 42; v = F.from_val (Sum_type2.B (-123)) };

    check_roundtrip
      force_lazy06
      Lazy06.pp Lazy06.write Lazy06.read
      { Lazy06.x = 42; v = F.from_val Sum_type2.A };

    check_roundtrip
      force_lazy06
      Lazy06.pp Lazy06.write Lazy06.read
      { Lazy06.x = 42; v = thunk (Sum_type2.D "1111") };

    check_roundtrip
      force_lazy06
      Lazy06.pp Lazy06.write Lazy06.read
      { Lazy06.x = 42; v = thunk Sum_type2.A };
  end;

  "sum types, compat with eager fields" >:: begin fun () ->
    check_roundtrip_complex
      force_lazy06
      Lazy06.pp Lazy06b.write Lazy06.read
      { Lazy06b.x = 42; v = (Sum_type2.B (-123)) }
      { Lazy06.x = 42; v = F.from_val (Sum_type2.B (-123)) };

    check_roundtrip_complex
      force_lazy06
      Lazy06.pp Lazy06b.write Lazy06.read
      { Lazy06b.x = 42; v = Sum_type2.A }
      { Lazy06.x = 42; v = F.from_val Sum_type2.A };

    check_roundtrip_complex
      nop
      Lazy06b.pp Lazy06.write Lazy06b.read
      { Lazy06.x = 42; v = F.from_val (Sum_type2.B (-123)) }
      { Lazy06b.x = 42; v = (Sum_type2.B (-123)) };

    check_roundtrip_complex
      nop
      Lazy06b.pp Lazy06.write Lazy06b.read
      { Lazy06.x = 42; v = F.from_val Sum_type2.A }
      { Lazy06b.x = 42; v = Sum_type2.A };

    check_roundtrip_complex
      force_lazy06
      Lazy06.pp Lazy06b.write Lazy06.read
      { Lazy06b.x = 42; v = (Sum_type2.B (-123)) }
      { Lazy06.x = 42; v = thunk (Sum_type2.B (-123)) };

    check_roundtrip_complex
      force_lazy06
      Lazy06.pp Lazy06b.write Lazy06.read
      { Lazy06b.x = 42; v = Sum_type2.A }
      { Lazy06.x = 42; v = thunk Sum_type2.A };

    check_roundtrip_complex
      nop
      Lazy06b.pp Lazy06.write Lazy06b.read
      { Lazy06.x = 42; v = thunk (Sum_type2.B (-123)) }
      { Lazy06b.x = 42; v = (Sum_type2.B (-123)) };

    check_roundtrip_complex
      nop
      Lazy06b.pp Lazy06.write Lazy06b.read
      { Lazy06.x = 42; v = thunk Sum_type2.A }
      { Lazy06b.x = 42; v = Sum_type2.A };
  end;

  "nested messages" >:: begin fun () ->
    check_roundtrip
      force_lazy10
      Lazy10.pp Lazy10.write Lazy10.read
      { Lazy10.x = 42; v = F.from_val { Simple_bool.v = true } };

    check_roundtrip
      force_lazy10
      Lazy10.pp Lazy10.write Lazy10.read
      { Lazy10.x = 42; v = thunk { Simple_bool.v = true } };

    check_roundtrip_complex
      force_lazy10
      Lazy10.pp Lazy10b.write Lazy10.read
      { Lazy10b.x = 42; v = {Simple_bool.v = true }; }
      { Lazy10.x = 42; v = F.from_val {Simple_bool.v = true }; };

    check_roundtrip_complex
      nop
      Lazy10b.pp Lazy10.write Lazy10b.read
      { Lazy10.x = 42; v = F.from_val {Simple_bool.v = true }; }
      { Lazy10b.x = 42; v = {Simple_bool.v = true }; };
  end;

  "complex type serialization compat" >:: begin fun () ->
    check_serialization_equiv
      Lazy16.write Lazy16b.write
      { Lazy16.
        v1 = F.from_val { Simple_bool.v = true };
        v2 = F.from_val (10, -42);
        v3 = F.from_val ("fooobar", 123456);
        v4 = F.from_val Sum_type.D;
        v5 = F.from_val (Sum_type.B "foo");
        v6 = F.from_val (Sum_type.A { Simple_bool.v = true });
        v7 = F.from_val ["a"; "b"];
        v8 = F.from_val [];
        v9 = F.from_val (Array.init 345 ((-) 100));
        v0 = F.from_val [||];
      }
      { Lazy16b.
        v1 = { Simple_bool.v = true };
        v2 = (10, -42);
        v3 = ("fooobar", 123456);
        v4 = Sum_type.D;
        v5 = (Sum_type.B "foo");
        v6 = (Sum_type.A { Simple_bool.v = true });
        v7 = ["a"; "b"];
        v8 = [];
        v9 = (Array.init 345 ((-) 100));
        v0 = [||];
      };

    check_serialization_equiv
      Lazy16.write Lazy16b.write
      { Lazy16.
        v1 = thunk { Simple_bool.v = true };
        v2 = thunk (10, -42);
        v3 = thunk ("fooobar", 123456);
        v4 = thunk Sum_type.D;
        v5 = thunk (Sum_type.B "foo");
        v6 = thunk (Sum_type.A { Simple_bool.v = true });
        v7 = thunk ["a"; "b"];
        v8 = thunk [];
        v9 = thunk (Array.init 345 ((-) 100));
        v0 = thunk [||];
      }
      { Lazy16b.
        v1 = { Simple_bool.v = true };
        v2 = (10, -42);
        v3 = ("fooobar", 123456);
        v4 = Sum_type.D;
        v5 = (Sum_type.B "foo");
        v6 = (Sum_type.A { Simple_bool.v = true });
        v7 = ["a"; "b"];
        v8 = [];
        v9 = (Array.init 345 ((-) 100));
        v0 = [||];
      }
  end

]

let () = Register_test.register "lazy fields"
  [
    tests
  ]
