(** Convenience functions to serialize and deserialize values to/from strings
  * and extlib IO channels. *)

(** [Wrong_protocol_version (max_known, found)] *)
exception Wrong_protocol_version of int * int

type ('a, 'hint, 'path) string_reader =
  ?hint:'hint -> ?level:int -> ?path:'path -> Reader.String_reader.t -> 'a

type ('a, 'hint, 'path) io_reader =
  ?hint:'hint -> ?level:int -> ?path:'path -> Reader.IO_reader.t -> 'a

type 'a writer = (Msg_buffer.t -> 'a -> unit)

val serialize : ?buf:Msg_buffer.t -> 'a writer -> 'a -> string
val deserialize : ('a, 'hint, 'path) string_reader -> ?offset:int -> string -> 'a

val read : ('a, 'hint, 'path) io_reader -> IO.input -> 'a

val dump : 'a writer -> Msg_buffer.t -> 'a -> unit

(** @param buf the buffer to use when serializing the value. It will be
  *            automatically cleared before use. *)
val write :
  ?buf:Msg_buffer.t ->
  'a writer -> 'b IO.output -> 'a -> unit

(** {6 Versioned serialization} *)

(* [ioread_versioned fs io] reads the version number as a little-endian, 16-bit,
 * unsigned integer and uses it to index the [fs] array of reader functions.
 * If the version number is not known, the message is discarded and
 * [Wrong_protocol_version] is raised. You can therefore try to read again
 * from the same reader later.
 * @raise Wrong_protocol_version if the version is higher than the last known
 * one.
 * *)
val read_versioned : ('a, 'hint, 'path) io_reader array -> Reader.IO_reader.t -> 'a

(** Like {!read_versioned}, operating on an [IO.input] channel. *)
val io_read_versioned : ('a, 'hint, 'path) io_reader array -> IO.input -> 'a

(** [write_versioned ?buf fs version io x] writes the given [version] as a
  * 16-bit, unsigned integer to [io], followed by the serialization of the
  * value as performed by the [version-th] function from the [fs] array. If
  * the serialization fails, nothing is written.
  * @raise Invalid_argument if the [version] is not a valid index into
  * the array.
  * *)
val write_versioned :
  ?buf:Msg_buffer.t ->
  'a writer array -> int -> 'b IO.output -> 'a -> unit

(** Analog to {!write_versioned}, returning a string. *)
val serialize_versioned : ?buf:Msg_buffer.t ->
  'a writer array -> int -> 'a -> string

(** Analog to {!read_versioned}, reading from a string. *)
val deserialize_versioned :
  ('a, 'hint, 'path) string_reader array -> string -> 'a

(** Analog to {!deserialize_versioned}, where the version is given, and the
  * string only contains the message (not the version number) *)
val deserialize_versioned' :
  ('a, 'hint, 'path) string_reader array -> int -> string -> 'a

(** Return frame (16-bit version plus message) *)
val read_frame : IO.input -> int * string
