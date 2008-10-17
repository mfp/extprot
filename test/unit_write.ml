
open OUnit

let () =
  Register_test.register "write (unit)"
    [
      "fail" >:: (fun () -> assert_bool "Always fails" false);
    ]
