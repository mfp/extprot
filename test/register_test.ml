open OUnit

let tests = ref []

let register name ts = tests := (name >::: ts) :: !tests

let tests () = "All tests" >::: !tests
