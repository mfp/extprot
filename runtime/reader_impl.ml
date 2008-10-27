
let read_prefix = read_vint

let check_prim_type ty t =
  let p = read_prefix t in
    if ll_tag p <> 0 then begin
      (* skip the rest of the value *)
      skip_value t p;
      Error.unknown_tag (ll_tag p)
    end;
    if ll_type p <> ty then begin
      (* skip the rest of the value *)
      skip_value t p;
      Error.bad_wire_type ()
    end

DEFINE Read_prim_type(t, ty, read) =
 let p = read_prefix t in
 let llty = ll_type p in
   if ll_tag p = 0 && llty = ty then read
   else if ll_tag p <> 0 then begin
     skip_value t p;
     Error.unknown_tag (ll_tag p)
   end else if llty = Tuple then begin
     let len = read_vint t in
     let eot = offset t len in
     let nelms = read_vint t in
       if nelms >= 1 then begin
         let p = read_prefix t in
           if ll_tag p <> 0 then begin
             skip_to t eot;
             Error.unknown_tag (ll_tag p)
           end;
           if ll_type p <> ty then begin
             skip_to t eot;
             Error.bad_wire_type ()
           end;
           let v = read in
             skip_to t eot;
             v
       end else begin
         skip_to t eot;
         Error.bad_wire_type ()
       end
   end else begin
      skip_value t p;
      Error.bad_wire_type ()
    end

let read_raw_bool t = match read_vint t with
    0 -> false
  | _ -> true

let read_bool t = Read_prim_type(t, Bits8, read_raw_bool t)

let read_raw_i8 t = read_byte t

let read_i8 t = Read_prim_type(t, Bits8, read_raw_i8 t)

let read_raw_rel_int t =
  let n = read_vint t in (n lsr 1) lxor (- (n land 1))

let read_rel_int t = Read_prim_type(t, Vint, read_raw_rel_int t)

let (+!) = Int32.add
let (<!) = Int32.shift_left
let to_i32 = Int32.of_int

let (+!!) = Int64.add
let (<!!) = Int64.shift_left
let to_i64 = Int64.of_int

let read_raw_i32 t =
  let a = read_byte t in
  let b = read_byte t in
  let c = read_byte t in
  let d = read_byte t in
    to_i32 a +! (to_i32 b <! 8) +! (to_i32 c <! 16) +! (to_i32 d <! 24)

let read_i32 t = Read_prim_type(t, Bits32, read_raw_i32 t)

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

let read_raw_i64 = read_i64_bits

let read_i64 t = Read_prim_type(t, Bits64_long, read_raw_i64 t)

let read_raw_float t = Int64.float_of_bits (read_i64_bits t)

let read_float t = Read_prim_type(t, Bits64_float, read_raw_float t)

let read_raw_string t =
  let len = read_vint t in
  let s = String.create len in
    read_bytes t s 0 len;
    s

let read_string t = Read_prim_type(t, Bytes, read_raw_string t)
