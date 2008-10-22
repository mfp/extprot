
type low_level_type =
    Vint
  | Bits32
  | Bits64
  | Bytes
  | Tuple
  | Htuple

type prefix = int

let vint_length = function
    n when n < 128 -> 1
  | n when n < 13384 -> 2
  | n when n < 2097152 -> 3
  | n when n < 268435456 -> 4
  | _ -> 5 (* FIXME: checking for 64-bit and 32-bit archs *)

let ll_type prefix = match prefix land 0xf with
  (* varlen = 0 *)
    0 (* ctyp 0 *) -> Vint
  | 2 (* ctyp 1 *) -> Bits32
  | 4 (* ctyp 2 *) -> Bits64
  (* varlen = 1 *)
  | 1 (* ctyp 0 *) -> Tuple
  | 3 (* ctyp 1 *) -> Bytes
  | 5 (* ctyp 2 *) -> Htuple
  | _ -> Error.bad_format ()

let tuple_prefix tag = (0x01 lor (tag lsl 4))  (* vlen:1 ctyp:0 *)
let htuple_prefix tag = (0x05 lor (tag lsl 4)) (* vlen:1 ctyp:2 *)
let const_prefix tag = (tag lsl 4)             (* vlen:0 ctyp:0 *)

(* all the following for tag 0 *)
let bool_prefix = 0                            (* ctyp 0, vlen 0 *)
let relative_int_prefix = 0                    (* ctyp 0, vlen 0 *)
let positive_int_prefix = 0                    (* ctyp 0, vlen 0 *)
let float_prefix = 4                           (* ctyp 2, vlen 0 *)
let int64_prefix = 4                           (* ctyp 2, vlen 0 *)
let string_prefix = 3                          (* ctyp 1, vlen 1 *)

let ll_tag prefix = prefix lsr 4
