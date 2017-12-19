open Codec

module type S =
sig
  type t
  type position

  val close : t -> unit

  val read_byte : t -> int

  val read_prefix : t -> prefix

  val read_vint : t -> int
  val read_bool : t -> bool
  val read_rel_int : t -> int
  val read_i8 : t -> int
  val read_i32 : t -> Int32.t
  val read_i64 : t -> Int64.t
  val read_float : t -> float
  val read_string : t -> string

  val read_raw_bool : t -> bool
  val read_raw_rel_int : t -> int
  val read_raw_i8 : t -> int
  val read_raw_i32 : t -> Int32.t
  val read_raw_i64 : t -> Int64.t
  val read_raw_float : t -> float
  val read_raw_string : t -> string

  val offset : t -> int -> position
  val skip_to : t -> position -> unit

  val skip_value : t -> prefix -> unit

  val read_message : t -> string
end

type reader_func =
    [
      | `Offset | `Skip_to | `Read_prefix | `Skip_value
      | `Read_vint | `Read_bool | `Read_rel_int | `Read_i8
      | `Read_i32 | `Read_i64 | `Read_float | `Read_string
      | `Read_raw_bool | `Read_raw_rel_int | `Read_raw_i8
      | `Read_raw_i32 | `Read_raw_i64 | `Read_raw_float | `Read_raw_string
    ]

let string_of_reader_func : reader_func -> string = function
  | `Offset -> "offset"
  | `Skip_to -> "skip_to"
  | `Skip_value -> "skip_value"
  | `Read_prefix -> "read_prefix"
  | `Read_vint -> "read_vint"
  | `Read_bool -> "read_bool"
  | `Read_rel_int -> "read_rel_int"
  | `Read_i8 -> "read_i8"
  | `Read_i32 -> "read_i32"
  | `Read_i64 -> "read_i64"
  | `Read_float -> "read_float"
  | `Read_string -> "read_string"
  | `Read_raw_bool -> "read_raw_bool"
  | `Read_raw_rel_int -> "read_raw_rel_int"
  | `Read_raw_i8 -> "read_raw_i8"
  | `Read_raw_i32 -> "read_raw_i32"
  | `Read_raw_i64 -> "read_raw_i64"
  | `Read_raw_float -> "read_raw_float"
  | `Read_raw_string -> "read_raw_string"

DEFINE Read_vint(t) =
  let b = ref (read_byte t) in if !b < 128 then !b else
  let x = ref 0 in
  let e = ref 0 in
    while !b >= 128 do
      x := !x + ((!b - 128) lsl !e);
      e := !e + 7;
      b := read_byte t
    done;
    !x + (!b lsl !e)

DEFINE Read_msg(t) =
  let p = read_prefix t in
    match ll_type p with
        Tuple ->
          let len = read_vint t in
          let prefix_s =
            let b = Msg_buffer.create () in
              Msg_buffer.add_vint b p;
              Msg_buffer.add_vint b len;
              Msg_buffer.contents b in
          let prefix_len = String.length prefix_s in
          let s = Bytes.create (prefix_len + len) in
            String.blit prefix_s 0 s 0 prefix_len;
            read_bytes t s prefix_len len;
            Bytes.unsafe_to_string s
      | Vint | Bits8 | Bits32 | Bits64_long | Bits64_float | Enum | Bytes
      | Htuple | Assoc | Invalid_ll_type as ll_type -> Error.bad_wire_type ~ll_type ()

open Printf

let input_string funcname offset s =
  let pos = ref offset in
  let len = String.length s in
    if !pos < 0 then
      invalid_arg (sprintf "%s: invalid offset %d" funcname offset)
    else
      IO.create_in
        ~read:(fun () ->
                 if !pos >= len then raise IO.No_more_input;
                 let c = String.unsafe_get s !pos in
                   incr pos;
                   c)
        ~input:(fun sout p l ->
                  if !pos >= len then raise IO.No_more_input;
                  let n = (if !pos + l > len then len - !pos else l) in
                    String.unsafe_blit s !pos sout p n;
                    pos := !pos + n;
                    n)
        ~close:(fun () -> ())

module IO_reader =
struct
  type t =  { io : IO.input; mutable pos : int }
  type position = int

  let read_bytes_is_atomic = false

  let from_io io = { io = io; pos = 0 }

  let from_string ?(offset = 0) s =
    { io = input_string "Extprot.IO_reader.from_string" offset s; pos = 0 }

  let from_file fname = from_io (IO.input_channel (open_in fname))

  let close t = IO.close_in t.io

  let offset t off =
    if off < 0 then invalid_arg "Extprot.Reader.IO_reader.offset";
    t.pos + off

  let read_byte t =
    let b = IO.read_byte t.io in
      t.pos <- t.pos + 1;
      b

  let read_bytes t buf off len =
    if off < 0 || len < 0 || off + len > Bytes.length buf then
      invalid_arg "Reader.IO_reader.read_bytes";
    let n = IO.really_input t.io buf off len in
      t.pos <- t.pos + n;
      if n <> len then raise End_of_file

  let read_vint t = Read_vint(t)

  let skip_buf = Bytes.create 4096

  let rec skip_n t = function
      0 -> ()
    | n -> let len = min n (Bytes.length skip_buf) in
        read_bytes t skip_buf 0 len;
        skip_n t (n - len)

  let skip_value t p = match ll_type p with
      Vint -> ignore (read_vint t)
    | Bits8 -> ignore (read_byte t)
    | Bits32 -> ignore (read_bytes t skip_buf 0 4)
    | Bits64_float | Bits64_long -> ignore (read_bytes t skip_buf 0 8)
    | Enum -> ()
    | Tuple | Htuple | Bytes | Assoc -> skip_n t (read_vint t)
    | Invalid_ll_type -> Error.bad_wire_type ()

  let skip_to t pos = if t.pos < pos then skip_n t (pos - t.pos)

  INCLUDE "reader_impl.ml"
  DEFINE EOF_wrap(f, x) = try f x with IO.No_more_input -> raise End_of_file

  let read_prefix t = EOF_wrap(read_prefix, t)
  let read_vint t = EOF_wrap(read_vint, t)
  let read_bool t = EOF_wrap(read_bool, t)
  let read_rel_int t = EOF_wrap(read_rel_int, t)
  let read_i8 t = EOF_wrap(read_i8, t)
  let read_i32 t = EOF_wrap(read_i32, t)
  let read_i64 t = EOF_wrap(read_i64, t)
  let read_float t = EOF_wrap(read_float, t)
  let read_string t = EOF_wrap(read_string, t)

  let read_message t = Read_msg(t)
end

module String_reader =
struct
  type t = { mutable buf : string; mutable last : int; mutable pos : int }
  type position = int

  let read_bytes_is_atomic = true

  let make s off len =
    if off < 0 || len < 0 || off + len > String.length s then
      invalid_arg "Reader.String_reader.make";
    { buf = s; pos = off; last = off + len }

  let from_string s = make s 0 (String.length s)

  let from_io_reader' ch =
    let hd = IO_reader.read_prefix ch in
      if Codec.ll_type hd <> Codec.Tuple then
        Error.bad_wire_type ~ll_type:(Codec.ll_type hd) ();
      let len = IO_reader.read_vint ch in
      let hd_len = Codec.vint_length hd in
      let len_len = Codec.vint_length len in
      (* could expose some
       * from_io_reader_non_threadsafe : ?Msg_buffer.t -> IO.input -> t
       * allowing to read into the same (msg)_buffer instead of allocating a
       * fresh one for each message -- then we'd have to resize the buffer
       * here (if needed) instead of allocating a new one  *)
      let m = Msg_buffer.make (hd_len + len_len + len) in
        Msg_buffer.add_vint m hd;
        Msg_buffer.add_vint m len;
        let off = Msg_buffer.length m in
        let buf = Msg_buffer.unsafe_contents m in
          IO_reader.read_bytes ch buf off len;
          (make (Bytes.unsafe_to_string buf) 0 (Bytes.length buf), Bytes.unsafe_to_string buf)

  let from_io_reader ch = fst (from_io_reader' ch)

  let from_io io = from_io_reader (IO_reader.from_io io)

  let close t = (* invalidate reader *)
    t.buf <- "";
    t.pos <- 1;
    t.last <- 0

  let read_byte t =
    let pos = t.pos in
      if pos < t.last then begin
        let r = Char.code (String.unsafe_get t.buf pos) in
          t.pos <- t.pos + 1;
          r
      end else raise End_of_file

  let read_bytes t buf off len =
    if off < 0 || len < 0 || off + len > Bytes.length buf then
      invalid_arg "Reader.String_reader.read_bytes";
    if len > t.last - t.pos then raise End_of_file;
    Bytes.blit_string t.buf t.pos buf off len;
    t.pos <- t.pos + len

  let read_vint t = Read_vint(t)

  let skip_value t p = match ll_type p with
      Vint  -> ignore (read_vint t)
    | Bits8 -> t.pos <- t.pos + 1
    | Bits32 -> t.pos <- t.pos + 4
    | Bits64_long | Bits64_float -> t.pos <- t.pos + 8
    | Enum -> ()
    | Tuple | Htuple | Bytes | Assoc -> let len = read_vint t in t.pos <- t.pos + len
    | Invalid_ll_type -> Error.bad_wire_type ()

  let offset t off =
    let pos = off + t.pos in
    (* only check if > because need to be able to skip to EOF, but not "past" it *)
      if off < 0 then invalid_arg "Extprot.Reader.String_reader.offset";
      if pos > t.last then raise End_of_file;
      pos

  let skip_to t pos =
    if pos > t.last then raise End_of_file;
    if pos > t.pos then t.pos <- pos

  INCLUDE "reader_impl.ml"

  let read_message t = Read_msg(t)
end
