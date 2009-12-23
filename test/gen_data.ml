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
    rand_list rand_len rand_bool >>= fun a ->
      return (n, a) in
  let a2_elm = rand_sum_type rand_int (rand_string rand_len) rand_int64 in
    rand_list rand_len a1_elm >>= fun a1 ->
    rand_list rand_len a2_elm >>= fun a2 ->
    return (Complex_rtt.A { Complex_rtt.A.a1 = a1; a2 = a2 })

let rtt_b =
  rand_bool >>= fun b1 ->
  rand_tuple2 (rand_string rand_len) (rand_list rand_len rand_int) >>= fun b2 ->
    return (Complex_rtt.B { Complex_rtt.B.b1 = b1; b2 = b2 })

let complex_rtt = rand_choice [ rtt_a; rtt_b ]

let rand_record fa fb =
  fa >>= fun a ->
  fb >>= fun b ->
    return { Record.a = a; b = b }

let rec_message = rand_record rand_int (rand_string rand_len)

let rec_fields =
  rec_message >>= fun msg ->
  rand_int >>= fun b ->
    return { Rec_fields.a = msg; b = b }

let rec_message_sum =
  let a =
    rand_record rand_int (rand_string rand_len) >>= fun r ->
      return (Rec_message_sum.A r) in
  let b =
    rand_record rand_int rand_int >>= fun r -> return (Rec_message_sum.B r)
  in
    rand_choice [ a; b ]

module Xml = struct
  open Printf
  module B = Buffer

  let open_tag t b = B.add_char b '<'; B.add_string b t; B.add_char b '>'
  let close_tag t b = B.add_string b "</"; B.add_string b t; B.add_char b '>'

  let add b fmt = kprintf (B.add_string b) fmt

  let tag t f b =
    open_tag t b;
    f b;
    close_tag t b

  let sum_type_to_xml f1 f2 f3 x b = match x with
      Sum_type.A x -> tag "sum_A" (f1 x) b
    | Sum_type.B x -> tag "sum_B" (f2 x) b
    | Sum_type.C x -> tag "sum_C" (f3 x) b
    | Sum_type.D -> tag "sum_D" (fun _ -> ()) b

  let tuple2_to_xml
        (f1 : 'a -> B.t -> unit)
        (f2 : 'b -> B.t -> unit)
        ((a, b) : 'a * 'b) =
    tag "tuple" (fun buf -> f1 a buf; f2 b buf)

  let list_to_xml (f : 'a -> B.t -> unit) (l : 'a list) =
    tag "htuple" (fun buf -> List.iter (fun x -> f x buf) l)

  let array_to_xml (f : 'a -> B.t -> unit) (a : 'a array) =
    tag "htuple" (fun buf -> Array.iter (fun x -> f x buf) a)

  let int_to_xml x = tag "int" (fun b -> add b "%d" x)
  let bool_to_xml x = tag "bool" (fun b -> add b "%s" (string_of_bool x))
  let string_to_xml x = tag "string" (fun b -> B.add_string b (Base64.str_encode x))
  let long_to_xml x = tag "long" (fun b -> add b "%s" (Int64.to_string x))

  open Complex_rtt

  let complex_rtt_to_xml x b = match x with
      A t -> tag "complex_rtt_A" begin fun b ->
        list_to_xml (tuple2_to_xml int_to_xml (list_to_xml bool_to_xml)) t.A.a1 b;
        list_to_xml (sum_type_to_xml int_to_xml string_to_xml long_to_xml) t.A.a2 b
      end b
    | B t -> tag "complex_rtt_A" begin fun b ->
        bool_to_xml t.B.b1 b;
        tuple2_to_xml string_to_xml (list_to_xml int_to_xml) t.B.b2 b;
      end b
end
