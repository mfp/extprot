
(* Based on OCaml's buffer.ml in the stdlib.
 * Copyright
 *   1999 Institut National de Recherche en Informatique et en Automatique
 *   2008 Mauricio Fernández <mfp@acm.org>
 * *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*   Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt  *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Extensible buffers *)

type t =
 {mutable buffer : string;
  mutable position : int;
  mutable length : int;
  mutable release : unit -> unit;
 }


let `Round_up get_bytes, _ =
  Buffer_pool.make_buffer_pool ~min_size:256 ~max_size:(4 * 1024 * 1024)
    Bytes.create
    (fun size ->
       if size < 65536 then 128
       else 2 * 4 * 1024 * 1024 / size)

let dummy_get_bytes n = (Bytes.create n, (fun () -> ()))

let if_not_disabled normal dummy =
  try
    ignore (Unix.getenv "EXTPROT_DISABLE_BUFFER_POOL");
    dummy
  with Not_found ->
    normal

let get_bytes = if_not_disabled get_bytes dummy_get_bytes

let make n =
 let n = if n < 1 then 1 else n in
 let n = if n > Sys.max_string_length then Sys.max_string_length / 2 else n in
 let buffer, release = get_bytes n in
 { buffer; position = 0; length = Bytes.length buffer; release; }

let create () = make 32

let contents b = String.sub b.buffer 0 b.position

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub"
  else begin
    let r = String.create len in
    String.blit b.buffer ofs r 0 len;
    r
  end

let nth b ofs =
  if ofs < 0 || ofs >= b.position then
   invalid_arg "Buffer.nth"
  else String.get b.buffer ofs

let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.buffer <- Bytes.create 8;
  b.length <- Bytes.length b.buffer;
  b.release ();
  b.position <- 0

let discard = reset

let round_to_pow2 n =
  let m = ref 1 in
    while !m < n do
      m := !m * 2;
    done;
    !m

let resize b more =
  let new_len = round_to_pow2 (b.position + more) in
  let new_buffer, release =
    if new_len <= Sys.max_string_length then
      get_bytes new_len
    else if b.position + more < Sys.max_string_length then
      (Bytes.create (b.position + more), (fun () -> ()))
    else
      failwith "Msg_buffer.resize: cannot grow buffer"
  in
    String.blit b.buffer 0 new_buffer 0 b.position;
    b.release ();
    b.buffer <- new_buffer;
    b.length <- Bytes.length new_buffer;
    b.release <- release

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  b.buffer.[pos] <- c;
  b.position <- pos + 1

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Buffer.add_substring";
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  String.blit s offset b.buffer b.position len;
  b.position <- new_position

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  String.blit s 0 b.buffer b.position len;
  b.position <- new_position

let add_buffer b bs =
  add_substring b bs.buffer 0 bs.position

let add_channel b ic len =
  if b.position + len > b.length then resize b len;
  really_input ic b.buffer b.position len;
  b.position <- b.position + len

let output_buffer oc b =
  output oc b.buffer 0 b.position

let output_buffer_to_io io b =
  ignore (IO.really_output io b.buffer 0 b.position : int)

let add_byte b n = add_char b (Char.unsafe_chr n)

let add_vint b n =
  let n = ref n in
    while !n land -128 <> 0 do
      add_byte b (128 + (!n land 0x7f));
      n := !n lsr 7
    done;
    add_byte b !n

let add_tuple_prefix b tag = add_vint b (Codec.tuple_prefix tag)

let add_htuple_prefix b tag = add_vint b (Codec.htuple_prefix tag)

let add_const_prefix b tag = add_vint b (Codec.const_prefix tag)

let write_bool b bool =
  add_vint b Codec.bool_prefix;
  add_byte b (if bool then 1 else 0)

let write_int8 b c =
  add_vint b Codec.byte_prefix;
  add_byte b c

let write_relative_int b n =
  add_vint b Codec.relative_int_prefix;
  add_vint b ((n lsl 1) lxor (n asr 63))

let (&!) = Int32.logand
let (&!!) = Int64.logand
let (>!) = Int32.shift_right
let (>!!) = Int64.shift_right

let write_int32 b n =
  add_vint b 2; (* tag 0, ctyp 1, vlen 0 *)
  add_byte b (Int32.to_int (n &! 0xFFl));
  add_byte b (Int32.to_int ((n >!8) &! 0xFFl));
  add_byte b (Int32.to_int ((n >! 16) &! 0xFFl));
  add_byte b (Int32.to_int ((n >! 24) &! 0xFFl))

let write_int64_bits b n =
  add_byte b (Int64.to_int (n &!! 0xFFL));
  add_byte b (Int64.to_int ((n >!! 8)  &!! 0xFFL));
  add_byte b (Int64.to_int ((n >!! 16) &!! 0xFFL));
  add_byte b (Int64.to_int ((n >!! 24) &!! 0xFFL));
  add_byte b (Int64.to_int ((n >!! 32) &!! 0xFFL));
  add_byte b (Int64.to_int ((n >!! 40) &!! 0xFFL));
  add_byte b (Int64.to_int ((n >!! 48) &!! 0xFFL));
  add_byte b (Int64.to_int ((n >!! 56) &!! 0xFFL))

let write_int64 b n =
  add_vint b Codec.int64_prefix;
  write_int64_bits b n

let write_float b fl =
  add_vint b Codec.float_prefix;
  IFDEF BIG_ENDIAN THEN
    let n = Int64.bits_of_float fl in
      add_byte b (Int64.to_int ((n >!! 56) &!! 0xFFL));
      add_byte b (Int64.to_int ((n >!! 48) &!! 0xFFL));
      add_byte b (Int64.to_int ((n >!! 40) &!! 0xFFL));
      add_byte b (Int64.to_int ((n >!! 32) &!! 0xFFL));
      add_byte b (Int64.to_int ((n >!! 24) &!! 0xFFL));
      add_byte b (Int64.to_int ((n >!! 16) &!! 0xFFL));
      add_byte b (Int64.to_int ((n >!! 8)  &!! 0xFFL));
      add_byte b (Int64.to_int (n &!! 0xFFL))
  ELSE
    write_int64_bits b (Int64.bits_of_float fl)
  END

let write_string b s =
  add_vint b Codec.string_prefix;
  add_vint b (String.length s);
  add_string b s

let with_buffer f t = f t.buffer 0 t.position

let unsafe_contents t = t.buffer
