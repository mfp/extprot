open Printf
open OUnit
open Test_types
open Test_util
module PP = Extprot.Pretty_print
module M = Extprot.Msg_buffer

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

let default_value_tests = "default_values" >::: [
  "sum type" >:: begin fun () ->
    check_roundtrip Msg2a.pp_msg2a Msg2.write_msg2 Msg2a.read_msg2a
      { Msg2.a = 123 } { Msg2a.a = 123; b = Sum_type.D }
  end;

  "nested message" >:: begin fun () ->
    check_roundtrip Msg2b.pp_msg2b Msg2.write_msg2 Msg2b.read_msg2b
      { Msg2.a = 123 } { Msg2b.a = 123; b = { Simple_sum.v = Sum_type.D } }
  end;

  "lists and arrays" >:: begin fun () ->
    check_roundtrip Msg2c.pp_msg2c Msg2.write_msg2 Msg2c.read_msg2c
      { Msg2.a = 123 } { Msg2c.a = 123; b = []; c = [||] };

    check_roundtrip Msg2c.pp_msg2c Msg2c0.write_msg2c0 Msg2c.read_msg2c
      { Msg2c0.a = 123; b = [1;2;3] } { Msg2c.a = 123; b = [1;2;3]; c = [||] };
  end;

  "tuples" >:: begin fun () ->
    let simple_sum = { Simple_sum.v = Sum_type.D } in
      check_roundtrip Msg3a.pp_msg3a Msg3.write_msg3 Msg3a.read_msg3a
        { Msg3.v0 = 123 } { Msg3a.v0 = 123; v1 = (simple_sum, simple_sum); v2 = [] };
  end;

  "bool" >:: begin fun () ->
    check_roundtrip Msg2d.pp_msg2d Msg2.write_msg2 Msg2d.read_msg2d
      { Msg2.a = 123 } { Msg2d.a = 123; b = false }
  end;
]

let () = Register_test.register "extensions"
  [
    "default msg constructor" >:: begin fun () ->
      check_roundtrip Msg1.pp_msg1 Msg1a.write_msg1a Msg1.read_msg1
        (Msg1a.A { Msg1a.A.a = 123 }) { Msg1.a = 123 };

      raises_extprot_err
        ~msg:"Use of unknown tag"
        (check_roundtrip Msg1.pp_msg1 Msg1a.write_msg1a Msg1.read_msg1
           (Msg1a.B { Msg1a.B.b = "123" }))
        { Msg1.a = 123 }
    end;

    "dropping unknown fields" >:: begin fun () ->
      check_roundtrip Msg1a.pp_msg1a Msg1b.write_msg1b Msg1a.read_msg1a
        (Msg1b.A { Msg1b.A.a = 123; a' = 42; }) (Msg1a.A { Msg1a.A.a = 123 });
    end;

    "dropping unknown fields (sequence)" >:: begin fun () ->
      let a = Array.init 100 (fun _ -> Random.int 0x3ffffff) in
      let b = M.create () in
        Array.iter
          (fun i -> Msg1b.write_msg1b b (Msg1b.A { Msg1b.A.a = i; a' = -i; }))
          a;
        let s = M.contents b in
        let io = Extprot.Reader.String_reader.from_string s in
          Array.iter
            (fun n -> match Msg1a.read_msg1a io with
                 Msg1a.B _ -> assert_failure "Should get Msg1a.A _."
               | Msg1a.A t -> assert_equal ~printer:string_of_int n t.Msg1a.A.a)
            a
    end;

    "tuple to primitive type" >:: begin fun () ->
      check_roundtrip Msg1.pp_msg1 Msg1c.write_msg1c Msg1.read_msg1
        { Msg1c.a = Int_or_stuff.Int 123 } { Msg1.a = 123 };

      raises_extprot_err
        ~msg:"Use of new tag in tuple type"
        (check_roundtrip Msg1.pp_msg1 Msg1c.write_msg1c Msg1.read_msg1
           { Msg1c.a = Int_or_stuff.Stuff "foo" })
        { Msg1.a = 123 };
    end;

    "primitive type to tuple" >:: begin fun () ->
      check_roundtrip Msg1c.pp_msg1c Msg1.write_msg1 Msg1c.read_msg1c
        { Msg1.a = 123 } { Msg1c.a = Int_or_stuff.Int 123 };

      let simple_sum_default = { Simple_sum.v = Sum_type.D } in
      let tup = (simple_sum_default, simple_sum_default) in

        check_roundtrip
          Msg1d.pp_msg1d Msg1.write_msg1 Msg1d.read_msg1d
          { Msg1.a = 123 }
          { Msg1d.a = (123, Color.Red, tup) }
    end;

    "primitive type to non-constant variant" >:: begin fun () ->
      let simple_sum_default = { Simple_sum.v = Sum_type.D } in
      let tup = (simple_sum_default, simple_sum_default) in

      check_roundtrip Msg1e.pp_msg1e Msg1.write_msg1 Msg1e.read_msg1e
        { Msg1.a = 123 }
        { Msg1e.a = Node2.Node (123, Color.Red, tup)  }
    end;

    "primitive type promoted to message" >:: begin fun () ->
      check_roundtrip
        Prim_promotion2.pp Prim_promotion0.write Prim_promotion2.read
        { Prim_promotion0.v = "foo" }
        { Prim_promotion2.v = { Prim_promotion1.v = "foo"; foo = Sum_type.D; } }
    end;

    "numeric type widening" >:: begin fun () ->
      check_roundtrip Widen2.pp_widen2 Widen1.write_widen1 Widen2.read_widen2
        { Widen.a = 1; b = 2; c = 3L; d = 4 }
        { Widen.a = 1; b = 2L; c = 3.; d = 4L };

      check_roundtrip Widen3.pp_widen3 Widen1.write_widen1 Widen3.read_widen3
        { Widen.a = 1; b = 2; c = 3L; d = 4 }
        { Widen.a = 1L; b = 2.; c = 3.; d = 4. };
    end;

    default_value_tests;
  ]
