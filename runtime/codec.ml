include Types

type prefix = int

let vint_length = function
    n when n < 128 -> 1
  | n when n < 13384 -> 2
  | n when n < 2097152 -> 3
  | n when n < 268435456 -> 4
  | _ -> 5 (* FIXME: checking for 64-bit and 32-bit archs *)

let ll_type_prefix_table =
  [|
    Vint;            Tuple;           Bits8;           Bytes;
    Bits32;          Htuple;          Bits64_long;     Invalid_ll_type;
    Bits64_float;    Invalid_ll_type; Invalid_ll_type; Invalid_ll_type;
    Invalid_ll_type; Invalid_ll_type; Invalid_ll_type; Invalid_ll_type;
  |]

let ll_type prefix = Array.unsafe_get ll_type_prefix_table (prefix land 0xf)

let ll_tag prefix = prefix lsr 4

let tuple_prefix tag = (0x01 lor (tag lsl 4))  (* vlen:1 ctyp:0 *)
let htuple_prefix tag = (0x05 lor (tag lsl 4)) (* vlen:1 ctyp:2 *)
let const_prefix tag = (tag lsl 4)             (* vlen:0 ctyp:0 *)

(* all the following for tag 0 *)
let relative_int_prefix = 0                    (* ctyp 0, vlen 0 *)
let bool_prefix = 2                            (* ctyp 1, vlen 0 *)
let byte_prefix = 2
let int32_prefix = 4                           (* ctyp 2, vlen 0 *)
let int64_prefix = 6                           (* ctyp 3, vlen 0 *)
let float_prefix = 8                           (* ctyp 4, vlen 0 *)

let string_prefix = 3                          (* ctyp 1, vlen 1 *)
