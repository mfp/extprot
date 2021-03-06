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
  aeq pp (force v) (force (decode dec (encode enc v)));
  (* Verify with 2 rounds of serialization + deserialization, so that
   * after the 1st one the thunks hold the cached serialized data when it
   * applies, and it is used to serialize again. *)
  aeq pp (force v) (force (decode dec (encode enc (decode dec (encode enc v)))))

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
  F.discard_packed t.LazyT.v;
  t

let force_lazy02b t =
  let t_ = force_lazy02 t in
  let _  = force_lazy02 t_ in
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

let force_lazy15 = function
  | Lazy15.A { Lazy15.A.v } as t ->
      ignore (F.force v);
      F.discard_packed v;
      t
  | Lazy15.B { Lazy15.B.v; _ } as t ->
      ignore (F.force v);
      F.discard_packed v;
      t

let force_lazy16c t =
  ignore (F.force t.Lazy16c.v1);
  ignore (F.force t.Lazy16c.v2);
  ignore (F.force t.Lazy16c.v5);
  ignore (F.force t.Lazy16c.v9);
  ignore (F.force t.Lazy16c.v0);
  F.discard_packed t.Lazy16c.v1;
  F.discard_packed t.Lazy16c.v2;
  F.discard_packed t.Lazy16c.v5;
  F.discard_packed t.Lazy16c.v9;
  F.discard_packed t.Lazy16c.v0;
  t

let force_lazy17 t =
  ignore (F.force t.Lazy17.v1);
  ignore (F.force t.Lazy17.v2);
  ignore (F.force t.Lazy17.v3);
  ignore (F.force t.Lazy17.v4);
  ignore (F.force t.Lazy17.v5);
  ignore (F.force t.Lazy17.v6);
  F.discard_packed t.Lazy17.v1;
  F.discard_packed t.Lazy17.v2;
  F.discard_packed t.Lazy17.v3;
  F.discard_packed t.Lazy17.v4;
  F.discard_packed t.Lazy17.v5;
  F.discard_packed t.Lazy17.v6;
  t

let force_lazy18 t =
  ignore (F.force t.Lazy18.v1);
  ignore (F.force t.Lazy18.v2);
  F.discard_packed t.Lazy18.v1;
  F.discard_packed t.Lazy18.v2;
  t

let force_lazy19 t =
  ignore (F.force t.Lazy19.v);
  F.discard_packed t.Lazy19.v;
  t

let force_lazy20b t =
  ignore (F.force (F.force t.Lazy20b.v1).LazyT.v);
  ignore (F.force (F.force t.Lazy20b.v2).LazyT.v);
  F.discard_packed t.Lazy20b.v1;
  F.discard_packed (F.force t.Lazy20b.v1).LazyT.v;
  F.discard_packed t.Lazy20b.v2;
  F.discard_packed (F.force t.Lazy20b.v2).LazyT.v;
  t

let force_lazy20d t =
  ignore (force_lazy20b (F.force t.Lazy20d.v2));
  F.discard_packed t.Lazy20d.v2;
  t

let force_lazy21b = function
  | Lazy21b.A { Lazy21a.v1; v2; v3; v4; v5; v6; v7; v8; v9; v0 } as t ->
      ignore (F.force v1);
      ignore (F.force v2);
      ignore (F.force v3);
      ignore (F.force v4);
      ignore (F.force v5);
      ignore (F.force v6);
      ignore (F.force v7);
      ignore (F.force v8);
      ignore (F.force v9);
      ignore (F.force v0);
      F.discard_packed v1;
      F.discard_packed v2;
      F.discard_packed v3;
      F.discard_packed v4;
      F.discard_packed v5;
      F.discard_packed v6;
      F.discard_packed v7;
      F.discard_packed v8;
      F.discard_packed v9;
      F.discard_packed v0;
      t
  | Lazy21b.B x as t ->
      ignore (force_lazyT x);
      t

let force_lazy22 t =
  ignore (F.force t.Lazy22.v);
  F.discard_packed t.Lazy22.v;
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

  "external types" >:: begin fun () ->
    check_roundtrip
      force_lazy22
      Lazy22.pp Lazy22.write Lazy22.read
      { Lazy22.v = F.from_val @@ Digest_type.from_string "afe43acd3a3b3b05242b4f38976eaf48a65" };

    check_roundtrip
      force_lazy22
      Lazy22.pp Lazy22.write Lazy22.read
      { Lazy22.v = thunk @@ Digest_type.from_string "afe43acd3a3b3b05242b4f38976eaf48a65" };

    check_serialization_equiv
      Lazy22.write Lazy22b.write
      { Lazy22.v = F.from_val @@ Digest_type.from_string "afe43acd3a3b3b05242b4f38976eaf48a65" }
      { Lazy22b.v = Digest_type.from_string "afe43acd3a3b3b05242b4f38976eaf48a65" };
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

    check_serialization_equiv
      Lazy02c.write Lazy02b.write
      { RecT.v = { RecT.v = 42 } }
      { LazyT.v = F.from_val { LazyT.v = F.from_val 42 } };

    check_roundtrip
      force_lazy02b
      Lazy02b.pp Lazy02b.write Lazy02b.read
      { LazyT.v = F.from_val { LazyT.v = F.from_val 42 } };
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

  "message variants" >:: begin fun () ->
    check_roundtrip
      force_lazy15
      Lazy15.pp Lazy15.write Lazy15.read
      (Lazy15.A { Lazy15.A.v = F.from_val true });

    check_roundtrip
      force_lazy15
      Lazy15.pp Lazy15.write Lazy15.read
      (Lazy15.B { Lazy15.B.x = 42; v = F.from_val (Sum_type2.B 12345)});

    check_roundtrip
      force_lazy15
      Lazy15.pp Lazy15.write Lazy15.read
      (Lazy15.A { Lazy15.A.v = thunk true });

    check_roundtrip
      force_lazy15
      Lazy15.pp Lazy15.write Lazy15.read
      (Lazy15.B { Lazy15.B.x = 42; v = thunk (Sum_type2.B 12345)});

    check_roundtrip_complex
      force_lazy15
      Lazy15.pp Lazy15b.write Lazy15.read
      (Lazy15b.A { Lazy15b.A.v = true })
      (Lazy15.A { Lazy15.A.v = F.from_val true });
  end;

  "subsets of messages with lazy fields" >:: begin fun () ->
    check_roundtrip_complex
      force_lazy16c
      Lazy16c.pp Lazy16.write Lazy16c.read
      { Lazy16.
        v1 = F.from_val { Simple_bool.v = true};
        v2 = F.from_val (-4343, "dfsdfsd");
        v3 = F.from_val ("xxxx", 111);
        v4 = F.from_val @@ Sum_type.B 666;
        v5 = thunk @@ Sum_type.B "hhhhh";
        v6 = F.from_val Sum_type.D;
        v7 = F.from_val [ "x"; "y" ];
        v8 = F.from_val @@ List.map (sprintf "%d") @@ Array.to_list @@ Array.init 100 ((+) 42);
        v9 = F.from_val [| 1; -66666 |];
        v0 = F.from_val @@ Array.init 100 ((-) 42);
      }
      { Lazy16c.
        v1 = F.from_val { Simple_bool.v = true};
        v2 = F.from_val (-4343, "dfsdfsd");
        v5 = thunk @@ Sum_type.B "hhhhh";
        v9 = F.from_val [| 1; -66666 |];
        v0 = F.from_val @@ Array.init 100 ((-) 42);
      };

    check_roundtrip_complex
      force_lazy16c
      Lazy16c.pp Lazy16b.write Lazy16c.read
      { Lazy16b.
        v1 = { Simple_bool.v = true};
        v2 = (-4343, "dfsdfsd");
        v3 = ("xxxx", 111);
        v4 = Sum_type.B 666;
        v5 = Sum_type.B "hhhhh";
        v6 = Sum_type.D;
        v7 = [ "x"; "y" ];
        v8 = List.map (sprintf "%d") @@ Array.to_list @@ Array.init 100 ((+) 42);
        v9 = [| 1; -66666 |];
        v0 = Array.init 100 ((-) 42);
      }
      { Lazy16c.
        v1 = F.from_val { Simple_bool.v = true};
        v2 = F.from_val (-4343, "dfsdfsd");
        v5 = thunk @@ Sum_type.B "hhhhh";
        v9 = F.from_val [| 1; -66666 |];
        v0 = F.from_val @@ Array.init 100 ((-) 42);
      };
  end;

  "subsets with type ascription" >:: begin fun () ->
    check_roundtrip_complex
      force_lazy20d
      Lazy20d.pp Lazy20c.write Lazy20d.read
      { Lazy20c.
        v1 =
          F.from_val
            { Lazy20a.
              v1 = { LazyT.v = F.from_val "xxx123" };
              v2 = { LazyT.v = F.from_val { Simple_bool.v = true } } };

        v2 =
          F.from_val
            { Lazy20a.
              v1 = { LazyT.v = F.from_val "434324234xxx123" };
              v2 = { LazyT.v = F.from_val { Simple_bool.v = false } } };
      }
      { Lazy20d.v2 =
          F.from_val
            { Lazy20b.
              v1 = F.from_val { LazyT.v = F.from_val "434324234xxx123" };
              v2 = F.from_val { LazyT.v = F.from_val { Simple_bool.v = false } } };
      }
  end;

  "complex type serialization compat" >:: begin fun () ->
    check_serialization_equiv
      Lazy16.write Lazy16b.write
      { Lazy16.
        v1 = F.from_val { Simple_bool.v = true };
        v2 = F.from_val (10, "-42");
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
        v2 = (10, "-42");
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
        v2 = thunk (10, "-42");
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
        v2 = (10, "-42");
        v3 = ("fooobar", 123456);
        v4 = Sum_type.D;
        v5 = (Sum_type.B "foo");
        v6 = (Sum_type.A { Simple_bool.v = true });
        v7 = ["a"; "b"];
        v8 = [];
        v9 = (Array.init 345 ((-) 100));
        v0 = [||];
      }
  end;

  "default values" >:: begin fun () ->
    check_roundtrip_complex
      force_lazy17
      Lazy17.pp Lazy17b.write Lazy17.read
      { Lazy17b.v1 = 13; v2 = "foo"; v3 = Sum_type.C 3;
        v4 = [ 4; 5 ]; v5 = [| 7; 8 |]; }
      { Lazy17.v1 = F.from_val 13; v2 = F.from_val "foo";
        v3 = F.from_val (Sum_type.C 3); v4 = F.from_val [ 4; 5 ];
        v5 = F.from_val [| 7; 8 |];
        v6 = F.from_val { Simple_bool.v = false } };

    check_roundtrip_complex
      force_lazy17
      Lazy17.pp Lazy17c.write Lazy17.read
      { Lazy17c.v1 = 13; v2 = "foo"; v3 = Sum_type.C 3;
        v4 = [ 4; 5 ]; }
      { Lazy17.v1 = F.from_val 13; v2 = F.from_val "foo";
        v3 = F.from_val (Sum_type.C 3); v4 = F.from_val [ 4; 5 ];
        v5 = F.from_val [| |];
        v6 = F.from_val { Simple_bool.v = false } };

    check_roundtrip_complex
      force_lazy17
      Lazy17.pp Lazy17d.write Lazy17.read
      { Lazy17d.v1 = 13; v2 = "foo"; v3 = Sum_type.C 3; }
      { Lazy17.v1 = F.from_val 13; v2 = F.from_val "foo";
        v3 = F.from_val (Sum_type.C 3); v4 = F.from_val [ ];
        v5 = F.from_val [| |];
        v6 = F.from_val { Simple_bool.v = false } };

    check_roundtrip_complex
      force_lazy17
      Lazy17.pp Lazy17e.write Lazy17.read
      { Lazy17e.v1 = 13; v2 = "foo"; }
      { Lazy17.v1 = F.from_val 13; v2 = F.from_val "foo";
        v3 = F.from_val Sum_type.D; v4 = F.from_val [ ];
        v5 = F.from_val [| |];
        v6 = F.from_val { Simple_bool.v = false } };

    check_roundtrip_complex
      force_lazy17
      Lazy17.pp Lazy17f.write Lazy17.read
      { Lazy17f.v1 = 13; }
      { Lazy17.v1 = F.from_val 13; v2 = F.from_val "hohoho";
        v3 = F.from_val Sum_type.D; v4 = F.from_val [ ];
        v5 = F.from_val [| |];
        v6 = F.from_val { Simple_bool.v = false } };

    check_roundtrip_complex
      force_lazy18
      Lazy18.pp Lazy18b.write Lazy18.read
      { Lazy18b.v1 = { Lazy17f.v1 = 13; } }
      { Lazy18.
        v1 =
          F.from_val @@
          { Lazy17.v1 = F.from_val 13; v2 = F.from_val "hohoho";
            v3 = F.from_val Sum_type.D; v4 = F.from_val [ ];
            v5 = F.from_val [| |];
            v6 = F.from_val { Simple_bool.v = false } };
        v2 =
          F.from_val @@
          { Lazy17.v1 = F.from_val 789; v2 = F.from_val "hohoho";
            v3 = F.from_val Sum_type.D; v4 = F.from_val [ ];
            v5 = F.from_val [| |];
            v6 = F.from_val { Simple_bool.v = false } };
      };

    check_roundtrip_complex
      force_lazy19
      Lazy19.pp Lazy19b.write Lazy19.read
      { Lazy19b.x = -32245; v = { LazyT.v = F.from_val "heh" } }
      { Lazy19.x = -32245; v = F.from_val { LazyT.v = F.from_val "heh" } };

    check_roundtrip_complex
      force_lazy19
      Lazy19.pp Lazy19c.write Lazy19.read
      { Lazy19c.x = -32245; }
      { Lazy19.x = -32245; v = F.from_val { LazyT.v = F.from_val "hohoho" } };

    check_roundtrip_complex
      force_lazy15
      Lazy15.pp Lazy15b.write Lazy15.read
      (Lazy15b.B { Lazy15b.B.x = 12345 })
      (Lazy15.B { Lazy15.B.x = 12345; v = F.from_val Sum_type2.A });
  end;

  "randomized testing" >:: begin fun () ->
    for i = 0 to 5000 do
      let v = Gen_data.generate Gen_data.rand_lazy21b in
        check_roundtrip
          force_lazy21b
          Lazy21b.pp Lazy21b.write Lazy21b.read
          v
    done
  end;

]

let () = Register_test.register "lazy fields"
  [
    tests
  ]
