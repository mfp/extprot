open Codec

module type S =
sig
  type t

  val read_prefix : t -> prefix

  val read_vint : t -> int
  val read_bool : t -> bool
  val read_rel_int : t -> int
  val read_positive_int : t -> int
  val read_i32 : t -> Int32.t
  val read_i64 : t -> Int64.t
  val read_float : t -> float
  val read_string : t -> string
end


module String_reader : sig
  include S
  val make : string -> int -> int -> t
end =
struct
  type t = { buf : string; last : int; mutable pos : int }

  let make s off len =
    if off < 0 || len < 0 || off + len > String.length s then
      invalid_arg "Reader.String_reader.make";
    { buf = s; pos = off; last = off + len }

  let read_byte t =
    if t.pos >= t.last then raise End_of_file;
    let r = Char.code t.buf.[t.pos] in
      t.pos <- t.pos + 1;
      r

  let read_bytes t buf off len =
    if off < 0 || len < 0 || off + len > String.length buf then
      invalid_arg "Reader.String_reader.read_bytes";
    if len > t.last - t.pos then raise End_of_file;
    String.blit t.buf t.pos buf off len;
    t.pos <- t.pos + len

  INCLUDE "reader_impl.ml"
end

module IO_reader : sig
  include S with type t = IO.input
end =
struct
  type t = IO.input
  let read_byte = IO.read_byte

  let read_bytes io buf off len =
    if off < 0 || len < 0 || off + len > String.length buf then
      invalid_arg "Reader.IO_reader.read_bytes";
    match IO.really_input io buf off len with
        n when n = len -> ()
      | _ -> raise End_of_file

  INCLUDE "reader_impl.ml"
  DEFINE EOF_wrap(f, x) = try f x with IO.No_more_input -> raise End_of_file

  let read_prefix t = EOF_wrap(read_prefix, t)
  let read_vint t = EOF_wrap(read_vint, t)
  let read_bool t = EOF_wrap(read_bool, t)
  let read_rel_int t = EOF_wrap(read_rel_int, t)
  let read_positive_int t = EOF_wrap(read_positive_int, t)
  let read_i32 t = EOF_wrap(read_i32, t)
  let read_i64 t = EOF_wrap(read_i64, t)
  let read_float t = EOF_wrap(read_float, t)
  let read_string t = EOF_wrap(read_string, t)
end
