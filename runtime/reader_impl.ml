
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

DEFINE Read_prim_type_no_prefix(t, p, ty, read_fast, read_fallback) =
 let llty = ll_type p in
   if ll_tag p = 0 && llty = ty then read_fast t
   else if ll_tag p <> 0 then begin
     skip_value t p;
     Error.unknown_tag (ll_tag p)
   end else if llty <> Tuple then
     read_fallback t p llty
   else begin (* read 1st element from tuple *)
     let len = read_vint t in
     let eot = offset t len in
     let nelms = read_vint t in
       if nelms >= 1 then begin
         let p = read_prefix t in
           if ll_tag p <> 0 then begin
             skip_to t eot;
             Error.unknown_tag (ll_tag p)
           end;
           match ll_type p with
               llty when llty = ty ->
                 let v = read_fast t in
                   skip_to t eot;
                   v
             | llty -> begin
                 try
                   read_fallback t p llty
                 with Error.Extprot_error _ as e ->
                   skip_to t eot;
                   raise e
               end
       end else begin
         skip_to t eot;
         Error.bad_wire_type ()
       end
   end

DEFINE Read_prim_type(t, ty, read, read_fallback) =
  let p = read_prefix t in
    Read_prim_type_no_prefix(t, p, ty, read, read_fallback)

let read_raw_bool t = match read_byte t with
    0 -> false
  | _ -> true

let nil_fallback t p llty =
  skip_value t p;
  Error.bad_wire_type ()

let read_bool t = Read_prim_type(t, Bits8, read_raw_bool, nil_fallback)

let read_raw_i8 t = read_byte t

let read_i8 t = Read_prim_type(t, Bits8, read_raw_i8, nil_fallback)

let read_raw_rel_int t =
  let n = read_vint t in (n lsr 1) lxor (- (n land 1))

let read_rel_int_fallback t p = function
    Bits8 -> read_raw_i8 t
  | Vint -> read_raw_rel_int t
  | Bits32 | Bits64_long | Bits64_float
  | Enum | Tuple | Bytes | Htuple | Assoc | Invalid_ll_type as ty -> nil_fallback t p ty

let read_rel_int t = Read_prim_type(t, Vint, read_raw_rel_int, read_rel_int_fallback)

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

let read_i32_fallback t p = function
    Bits8 -> Int32.of_int (read_raw_i8 t)
  | Vint -> Int32.of_int (read_raw_rel_int t)
  | Bits32 -> read_raw_i32 t
  | Bits64_long | Bits64_float
  | Enum | Tuple | Bytes | Htuple | Assoc | Invalid_ll_type as ty -> nil_fallback t p ty

let read_i32 t = Read_prim_type(t, Bits32, read_raw_i32, read_i32_fallback)

let i64_buf = "12345678"

let load_i64_buf =
  (* if read_bytes is atomic, we can use a shared buffer as long as we are
   * careful afterwards and don't allow a context switch before we read its
   * contents *)
  if read_bytes_is_atomic then
    (fun t ->
      read_bytes t i64_buf 0 8;
      i64_buf)
  else
    (* otherwise, allocate a new buf *)
    (fun t ->
       let buf = String.create 8 in
         read_bytes t buf 0 8;
         buf)

let i64_byte_n buf n = Char.code (String.unsafe_get buf n)

let read_i64_bits t =
  let buf = load_i64_buf t in
  (* we read the bits without causing any allocation, thus preventing a
   * context switch, so it's OK if buf is allocated "statically" and reused
   * when load_i64_buf decides to *)
  let x1 = i64_byte_n buf 0 + (i64_byte_n buf 1 lsl 8) + (i64_byte_n buf 2 lsl 16) in
  let x2 = i64_byte_n buf 3 + (i64_byte_n buf 4 lsl 8) + (i64_byte_n buf 5 lsl 16) in
  let x3 = i64_byte_n buf 6 + (i64_byte_n buf 7 lsl 8) in
    Int64.add
      (Int64.add
         (Int64.of_int x1)
         (Int64.shift_left (Int64.of_int x2) 24))
      (Int64.shift_left (Int64.of_int x3) 48)

let read_raw_i64 = read_i64_bits

let read_i64_fallback t p = function
    Bits8 -> Int64.of_int (read_raw_i8 t)
  | Vint -> Int64.of_int (read_raw_rel_int t)
  | Bits32 -> Int64.of_int32 (read_raw_i32 t)
  | Bits64_long -> read_raw_i64 t
  | Bits64_float | Enum | Tuple | Bytes | Htuple | Assoc | Invalid_ll_type as ty ->
      nil_fallback t p ty

let read_i64 t = Read_prim_type(t, Bits64_long, read_raw_i64, read_i64_fallback)

let read_raw_float t =
  IFDEF BIG_ENDIAN THEN
    let a = read_byte t in
    let b = read_byte t in
    let c = read_byte t in
    let d = read_byte t in
    let e = read_byte t in
    let f = read_byte t in
    let g = read_byte t in
    let h = read_byte t in
      Int64.float_of_bits
        ( to_i64 h         +!! (to_i64 g <!! 8) +!!
         (to_i64 f <!! 16) +!! (to_i64 e <!! 24) +!!
         (to_i64 d <!! 32) +!! (to_i64 c <!! 40) +!!
         (to_i64 b <!! 48) +!! (to_i64 a <!! 56))
  ELSE
    Int64.float_of_bits (read_i64_bits t)
  END

let read_float_fallback t p = function
    Bits8 -> float (read_raw_i8 t)
  | Vint -> float (read_raw_rel_int t)
  | Bits32 -> Int32.to_float (read_raw_i32 t)
  | Bits64_long -> Int64.to_float (read_raw_i64 t)
  | Bits64_float -> read_raw_float t
  | Enum | Tuple | Bytes | Htuple | Assoc | Invalid_ll_type as ty -> nil_fallback t p ty

let read_float t = Read_prim_type(t, Bits64_float, read_raw_float, read_float_fallback)

let read_raw_string t =
  let len = read_vint t in
  let s = String.create len in
    read_bytes t s 0 len;
    s

let read_string t = Read_prim_type(t, Bytes, read_raw_string, nil_fallback)
