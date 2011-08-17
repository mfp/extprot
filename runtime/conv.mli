(** Convenience functions to serialize and deserialize values to/from strings
  * and extlib IO channels. *)

(** [Wrong_protocol_version (max_known, found)] *)
exception Wrong_protocol_version of int * int

val serialize : ?buf:Msg_buffer.t -> (Msg_buffer.t -> 'a -> 'b) -> 'a -> string
val deserialize : (Reader.String_reader.t -> 'a) -> ?offset:int -> string -> 'a

val read : (Reader.IO_reader.t -> 'a) -> IO.input -> 'a

val dump : (Msg_buffer.t -> 'a -> unit) -> Msg_buffer.t -> 'a -> unit

(** @param buf the buffer to use when serializing the value. It will be
  *            automatically cleared before use. *)
val write :
  ?buf:Msg_buffer.t ->
  (Msg_buffer.t -> 'a -> unit) -> 'b IO.output -> 'a -> unit

(** {6 Versioned serialization} *)

(* [ioread_versioned fs io] reads the version number as a little-endian, 16-bit,
 * unsigned integer and uses it to index the [fs] array of reader functions.
 * If the version number is not known, the message is discarded and
 * [Wrong_protocol_version] is raised. You can therefore try to read again
 * from the same reader later.
 * @raise Wrong_protocol_version if the version is higher than the last known
 * one.
 * *)
val read_versioned : (Reader.IO_reader.t -> 'a) array -> Reader.IO_reader.t -> 'a

(** Like {!read_versioned}, operating on an [IO.input] channel. *)
val io_read_versioned : (Reader.IO_reader.t -> 'a) array -> IO.input -> 'a

(** [write_versioned ?buf fs version io x] writes the given [version] as a
  * 16-bit, unsigned integer to [io], followed by the serialization of the
  * value as performed by the [version-th] function from the [fs] array. If
  * the serialization fails, nothing is written.
  * @raise Invalid_argument if the [version] is not a valid index into
  * the array.
  * *)
val write_versioned :
  ?buf:Msg_buffer.t ->
  (Msg_buffer.t -> 'a -> unit) array -> int -> 'b IO.output -> 'a -> unit

(** Analog to {!write_versioned}, returning a string. *)
val serialize_versioned : ?buf:Msg_buffer.t ->
  (Msg_buffer.t -> 'a -> unit) array -> int -> 'a -> string

(** Analog to {!read_versioned}, reading from a string. *)
val deserialize_versioned :
  (Reader.String_reader.t -> 'a) array -> string -> 'a

(** Analog to {!deserialize_versioned}, where the version is given, and the
  * string only contains the message (not the version number) *)
val deserialize_versioned' :
  (Reader.String_reader.t -> 'a) array -> int -> string -> 'a

(** Return frame (16-bit version plus message) *)
val read_frame : IO.input -> int * string
