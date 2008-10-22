
let read_prefix = read_vint

let check_prim_type ty t =
  let p = read_prefix t in
    if ll_tag p <> 0 || ll_type p <> ty then begin
      (* skip the rest of the value *)
      skip_value t p;
      Error.bad_format ()
    end

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
    (n lsr 1) lxor (- (n land 1))

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
  check_prim_type Bytes t;
  let len = read_vint t in
  let s = String.create len in
    read_bytes t s 0 len;
    s
