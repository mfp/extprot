
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
  initial_buffer : string}

let make n =
 let n = if n < 1 then 1 else n in
 let n = if n > Sys.max_string_length then Sys.max_string_length else n in
 let s = String.create n in
 {buffer = s; position = 0; length = n; initial_buffer = s}

(* TODO: obtain histogram of lengths for typical structures, pick default size
 * accordingly *)
let create () = make 8

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
  b.position <- 0; b.buffer <- b.initial_buffer;
  b.length <- String.length b.buffer

let resize b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do new_len := 2 * !new_len done;
  if !new_len > Sys.max_string_length then begin
    if b.position + more <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Buffer.add: cannot grow buffer"
  end;
  let new_buffer = String.create !new_len in
  String.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len

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

let add_byte b n = add_char b (Char.unsafe_chr n)

let add_vint b n =
  let n = ref n in
    while !n land -128 <> 0 do
      add_byte b (128 + (!n land 0x7f));
      n := !n lsr 7
    done;
    add_byte b !n

let add_tuple_prefix b tag =
  (* vlen:1 ctyp:0 *)
  add_vint b (0x01 lor (tag lsl 4))

let add_htuple_prefix b tag =
  (* vlen:1 ctyp:2 *)
  add_vint b (0x05 lor (tag lsl 4))

(* FIXME: review *)
let add_const_prefix b tag =
  (* vlen:0 ctyp:0 *)
  add_vint b (tag lsl 4)

let write_bool b bool =
  add_vint b 0; (* tag 0, ctyp 0, vlen 0 *)
  add_vint b (if bool then 1 else 0)

let write_relative_int b n =
  (* cannot just use (n lsl 1) lxor (n asr 63) because it causes problems with
   * large negative values *)
  let zigzag_pos n = n lsl 1 in
  let zigzag_neg n =
    (* this is OK because we're given a 63-bit int, so even after shift_left  it
     * will fit in 64 bits *)
    let n = Int64.of_int n in
      Int64.logxor (Int64.shift_left n 1) (Int64.shift_right n 63)
  in
    add_vint b 0; (* tag 0, ctyp 0, vlen 0 *)
    if n > 0 then
      add_vint b (zigzag_pos n)
    else
      (* TODO: find more efficient way to do this *)
      let n = ref (zigzag_neg n) in
        while Int64.logand !n (-128L) <> 0L do
          add_byte b (Int64.to_int (Int64.add 128L (Int64.logand !n 0x7fL)));
          n := Int64.shift_right_logical !n 7
        done;
        add_byte b (Int64.to_int !n)

let write_positive_int b n =
  add_vint b 0; (* tag 0, ctyp 0, vlen 0 *)
  add_vint b n

let write_int32 b n =
  add_vint b 2; (* tag 0, ctyp 1, vlen 0 *)
  add_byte b (Int32.to_int (Int32.logand n 0xFFl));
  add_byte b (Int32.to_int (Int32.logand (Int32.shift_right n 8) 0xFFl));
  add_byte b (Int32.to_int (Int32.logand (Int32.shift_right n 16) 0xFFl));
  add_byte b (Int32.to_int (Int32.logand (Int32.shift_right n 24) 0xFFl))

let write_int64_bits b n =
  add_byte b (Int64.to_int (Int64.logand n 0xFFL));
  add_byte b (Int64.to_int (Int64.logand (Int64.shift_right n 8) 0xFFL));
  add_byte b (Int64.to_int (Int64.logand (Int64.shift_right n 16) 0xFFL));
  add_byte b (Int64.to_int (Int64.logand (Int64.shift_right n 24) 0xFFL));
  add_byte b (Int64.to_int (Int64.logand (Int64.shift_right n 32) 0xFFL));
  add_byte b (Int64.to_int (Int64.logand (Int64.shift_right n 40) 0xFFL));
  add_byte b (Int64.to_int (Int64.logand (Int64.shift_right n 48) 0xFFL));
  add_byte b (Int64.to_int (Int64.logand (Int64.shift_right n 56) 0xFFL))

let write_int64 b n =
  add_vint b 4; (* tag 0, ctyp 2, vlen 0 *)
  write_int64_bits b n

let write_float b fl = write_int64 b (Int64.bits_of_float fl)

let write_string b s =
  add_vint b 0x3; (* tag 0, ctyp 1, vlen 1 *)
  add_vint b (String.length s);
  add_string b s
