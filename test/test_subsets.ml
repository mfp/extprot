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

let tests = "subsets" >::: [
  "basic functionality" >:: begin fun () ->
    check_roundtrip
      Subset__a_1.pp Subset__a.write Subset__a_1.read
      { Subset__a.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__a_1.b = true; d = 42L };

    check_roundtrip
      Subset__a_1b.pp Subset__a.write Subset__a_1b.read
      { Subset__a.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__a_1b.b = true; d = 42L };

    check_roundtrip
      Subset__a_2.pp Subset__a.write Subset__a_2.read
      { Subset__a.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__a_2.a = 13; c = "foo" };

    check_roundtrip
      Subset__a_2b.pp Subset__a.write Subset__a_2b.read
      { Subset__a.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__a_2b.a = 13; c = "foo" };
  end;

  "record types (monomorphic)" >:: begin fun () ->
    check_roundtrip
      Subset__b_1.pp Subset__b.write Subset__b_1.read
      { Subset__b_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__b_1.b = true; d = 42L };

    check_roundtrip
      Subset__b_1b.pp Subset__b.write Subset__b_1b.read
      { Subset__b_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__b_1b.b = true; d = 42L };

    check_roundtrip
      Subset__b_2.pp Subset__b.write Subset__b_2.read
      { Subset__b_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__b_2.a = 13; c = "foo" };

    check_roundtrip
      Subset__b_2b.pp Subset__b.write Subset__b_2b.read
      { Subset__b_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__b_2b.a = 13; c = "foo" };
  end;

  "record types (polymorphic)" >:: begin fun () ->
    check_roundtrip
      Subset__c_1.pp Subset__c.write Subset__c_1.read
      { Subset__c_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__c_1.b = true; d = 42L };

    check_roundtrip
      Subset__c_1b.pp Subset__c.write Subset__c_1b.read
      { Subset__c_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__c_1b.b = true; d = 42L };

    check_roundtrip
      Subset__c_2.pp Subset__c.write Subset__c_2.read
      { Subset__c_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__c_2.a = 13; c = "foo" };

    check_roundtrip
      Subset__c_2b.pp Subset__c.write Subset__c_2b.read
      { Subset__c_0.a = 13; b = true; c = "foo"; d = 42L }
      { Subset__c_2b.a = 13; c = "foo" };
  end
]

let () = Register_test.register "subsets"
  [
    tests
  ]