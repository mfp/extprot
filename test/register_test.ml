open OUnit

let tests = ref []

let rec wrap_test = function
    TestCase f -> TestCase (Util.unwrap_extprot_error f)
  | TestList l -> TestList (List.map wrap_test l)
  | TestLabel (lbl, t) -> TestLabel (lbl, wrap_test t)

let register name ts = tests := (name >::: List.map wrap_test ts) :: !tests

let tests () = "All tests" >::: List.rev !tests
