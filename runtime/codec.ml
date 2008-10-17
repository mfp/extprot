
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

let ll_tag prefix = prefix lsr 4

module Reader =
struct
  type t = { buf : string; last : int; mutable pos : int }

  let make s off len =
    if off < 0 || len < 0 || off + len > String.length s then
      invalid_arg "Codec.Reader.make";
    { buf = s; pos = off; last = off + len }

  let read_byte t =
    if t.pos >= t.last then raise End_of_file;
    let r = Char.code t.buf.[t.pos] in
      t.pos <- t.pos + 1;
      r

  let read_bytes t buf off len =
    if off < 0 || len < 0 || off + len < String.length buf then
      invalid_arg "Codec.Reader.read_bytes";
    if len > t.last - t.pos then raise End_of_file;
    String.blit t.buf t.pos buf off len;
    t.pos <- t.pos + len

  let read_vint t =
    let b = ref (read_byte t) in
    let x = ref 0 in
    let e = ref 0 in
      while !b >= 128 do
        x := !x + ((!b - 128) lsl !e);
        e := !e + 7;
        b := read_byte t
      done;
      !x + (!b lsl !e)

  let read_prefix = read_vint

  let check_prim_type ty t =
    let p = read_prefix t in
      if ll_tag p <> 0 || ll_type p <> ty then Error.bad_format ()

  let read_bool t =
    check_prim_type Vint t;
    match read_vint t with
        0 -> false
      | _ -> true

  let read_positive_int t =
    check_prim_type Vint t;
    read_vint t

  let read_rel_int t =
    check_prim_type Vint t;
    let n = read_vint t in
      if n land 1 = 0 then (n lsr 1) else ((-n) asr 1)

  let (+!) = Int32.add
  let (<!) = Int32.shift_left
  let to_i32 = Int32.of_int

  let (+!!) = Int64.add
  let (<!!) = Int64.shift_left
  let to_i64 = Int64.of_int

  let read_i32 t =
    check_prim_type Bits32 t;
    let a = read_byte t in
    let b = read_byte t in
    let c = read_byte t in
    let d = read_byte t in
      to_i32 a +! (to_i32 b <! 8) +! (to_i32 c <! 16) +! (to_i32 d <! 24)

  let read_i64_bits t =
    let a = read_byte t in
    let b = read_byte t in
    let c = read_byte t in
    let d = read_byte t in
    let e = read_byte t in
    let f = read_byte t in
    let g = read_byte t in
    let h = read_byte t in
      to_i64 a          +!! (to_i64 b <!! 8) +!!
      (to_i64 c <!! 16) +!! (to_i64 d <!! 24) +!!
      (to_i64 e <!! 32) +!! (to_i64 f <!! 40) +!!
      (to_i64 g <!! 48) +!! (to_i64 h <!! 56)

  let read_i64 t =
    check_prim_type Bits64 t;
    read_i64_bits t

  let read_float t =
    check_prim_type Bits64 t;
    Int64.float_of_bits (read_i64_bits t)

  let read_string t =
    let prefix = read_prefix t in
      if ll_tag prefix <> 0 || ll_type prefix <> Bytes then
        Error.bad_format ();
      let len = read_vint t in
      let s = String.create len in
        read_bytes t s 0 len;
        s
end

include Reader
