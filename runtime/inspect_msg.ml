open Codec
module PP = Pretty_print

let read_byte = IO.read_byte

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

let (+!) = Int32.add
let (<!) = Int32.shift_left
let to_i32 = Int32.of_int

let (+!!) = Int64.add
let (<!!) = Int64.shift_left
let to_i64 = Int64.of_int

let zigzag_dec n = (n lsr 1) lxor (- (n land 1))

let read_64_bits io =
  let a = read_byte io in
  let b = read_byte io in
  let c = read_byte io in
  let d = read_byte io in
  let e = read_byte io in
  let f = read_byte io in
  let g = read_byte io in
  let h = read_byte io in
    to_i64 a          +!! (to_i64 b <!! 8) +!!
    (to_i64 c <!! 16) +!! (to_i64 d <!! 24) +!!
    (to_i64 e <!! 32) +!! (to_i64 f <!! 40) +!!
    (to_i64 g <!! 48) +!! (to_i64 h <!! 56)

let pp_verbose fmt1 fmt2 f ~verbose pp ~tag x =
  if verbose || tag <> 0 then
    PP.fprintf pp fmt1 tag f x
  else
    PP.fprintf pp fmt2 f x

let rec inspect ?(verbose=true) pp io prefix =
  let tag = ll_tag prefix in match ll_type prefix with
    Tuple -> inspect_tuple ~verbose "{" "}" prefix pp io
  | Htuple -> inspect_tuple ~verbose "[" "]" prefix pp io
  | Vint ->
      let n = zigzag_dec (read_vint io) in
        pp_verbose "Vint_%d@[<1>@ %a@]" "%a" PP.pp_int ~verbose ~tag pp n
  | Bits8 ->
      let n = read_byte io in
        pp_verbose "I8_%d@[<1>@ %a@]" "0x%a"
          (fun pp n -> PP.fprintf pp "%X" n) ~verbose ~tag pp n
  | Bits32 ->
      let a = read_byte io in
      let b = read_byte io in
      let c = read_byte io in
      let d = read_byte io in
      let i32 = to_i32 a +! (to_i32 b <! 8) +! (to_i32 c <! 16) +! (to_i32 d <! 24) in
        pp_verbose "I32_%d@[<1>@ %a@]" "%al" PP.pp_int32 ~verbose ~tag pp i32
  | Bits64_long ->
      let n = read_64_bits io in
        pp_verbose "I64_%d@[<1>@ %a@]" "%aL" PP.pp_int64 ~verbose ~tag pp n
  | Bits64_float ->
      let fl = Int64.float_of_bits (read_64_bits io) in
        pp_verbose "Fl_%d@[<1>@ %a@]" "%a" PP.pp_float ~verbose ~tag pp fl
  | Enum ->
      if verbose then PP.fprintf pp "Enum_%d" tag else PP.fprintf pp "T%d" tag
  | Bytes ->
      let len = read_vint io in
      let s = IO.nread io len in
        pp_verbose "B_%d@[<1>@ %a@]" "%a" PP.pp_string ~verbose ~tag pp s
  | Invalid_ll_type -> Error.bad_wire_type ()

and inspect_tuple ?(verbose = true) left right prefix pp io =
  let tag = ll_tag prefix in
  let _ = read_vint io in (* len *)
  let nelms = read_vint io in
    if verbose || tag <> 0 then
      PP.fprintf pp "T%d %s@[<1>@ %a @]%s" tag
        left (inspect_elms ~verbose io) nelms right
    else
      PP.fprintf pp "%s@[<1>@ %a @]%s" left (inspect_elms ~verbose io) nelms right

and inspect_elms ?verbose io pp = function
    0 -> ()
  | 1 -> inspect ?verbose pp io (read_prefix io)
  | n -> inspect ?verbose pp io (read_prefix io);
         PP.fprintf pp ";@ ";
         inspect_elms ?verbose io pp (n - 1)

let inspect ?(verbose = true) pp io = inspect ~verbose pp io (read_prefix io)

