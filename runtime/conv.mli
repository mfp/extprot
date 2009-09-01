(** Convenience functions to serialize and deserialize values to/from strings
  * and extlib IO channels. *)

(** [Wrong_protocol_version (max_known, found)] *)
exception Wrong_protocol_version of int * int

val serialize : ?buf:Msg_buffer.t -> (Msg_buffer.t -> 'a -> 'b) -> 'a -> string
val deserialize : (Reader.String_reader.t -> 'a) -> string -> 'a

val read : (Reader.IO_reader.t -> 'a) -> IO.input -> 'a

(** @param buf the buffer to use when serializing the value. It will be
  *            automatically cleared before use. *)
val write :
  ?buf:Msg_buffer.t ->
  (Msg_buffer.t -> 'a -> unit) -> 'b IO.output -> 'a -> unit

(** {6 Versioned serialization} *)

(* [read_versioned fs io] reads a little-endian, 16-bit, unsigned integer
 * and uses it to index the [fs] array of reader functions.
 * @raise Wrong_protocol_version if the version is higher than the last known
 * one.
 * *)
val read_versioned : (Reader.IO_reader.t -> 'a) array -> IO.input -> 'a

(** [write_versioned ?buf version f io x] writes the given [version] as a
  * 16-bit, unsigned integer to [io], followed by the serialization of the
  * value as performed by [f]. If the serialization fails, nothing is written.
  * *)
val write_versioned :
  ?buf:Msg_buffer.t ->
  int -> (Msg_buffer.t -> 'a -> unit) -> 'b IO.output -> 'a -> unit

(** Analog to {!write_versioned}, returning a string. *)
val serialize_versioned :
  ?buf:Msg_buffer.t -> int -> (Msg_buffer.t -> 'a -> 'b) -> 'a -> string

(** Analog to {!read_versioned}, reading from a string. *)
val deserialize_versioned :
  (Reader.String_reader.t -> 'a) array -> string -> 'a
