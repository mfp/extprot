open Extprot.Random_gen
open Test_types

let generate = run
let rand_len = rand_integer 10
(* let rand_len = return 1 *)

let rand_sum_type a b c =
  rand_choice
    [
      (a >>= fun n -> return (Sum_type.A n));
      (b >>= fun n -> return (Sum_type.B n));
      (c >>= fun n -> return (Sum_type.C n));
      return Sum_type.D;
    ]

let rtt_a =
  let a1_elm =
    rand_int >>= fun n ->
    rand_array rand_len rand_bool >>= fun a ->
      return (n, a) in
  let a2_elm = rand_sum_type rand_int (rand_string rand_len) rand_int64 in
    rand_list rand_len a1_elm >>= fun a1 ->
    rand_list rand_len a2_elm >>= fun a2 ->
    return (Complex_rtt.A { Complex_rtt.a1 = a1; a2 = a2 })

let rtt_b =
  rand_bool >>= fun b1 ->
  rand_tuple2 (rand_string rand_len) (rand_list rand_len rand_int) >>= fun b2 ->
    return (Complex_rtt.B { Complex_rtt.b1 = b1; b2 = b2 })

let complex_rtt = rand_choice [ rtt_a; rtt_b ]

