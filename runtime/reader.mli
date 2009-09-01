module type S =
sig
  type t
  type position
  val close : t -> unit
  val read_prefix : t -> Codec.prefix
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
  val skip_value : t -> Codec.prefix -> unit
end

type reader_func =
    [ `Offset
    | `Read_bool
    | `Read_float
    | `Read_i32
    | `Read_i64
    | `Read_i8
    | `Read_prefix
    | `Read_raw_bool
    | `Read_raw_float
    | `Read_raw_i32
    | `Read_raw_i64
    | `Read_raw_i8
    | `Read_raw_rel_int
    | `Read_raw_string
    | `Read_rel_int
    | `Read_string
    | `Read_vint
    | `Skip_to ]

val string_of_reader_func : reader_func -> string

module IO_reader : sig
  include S
  val from_io : IO.input -> t
  val from_string : ?offset:int -> string -> t
  val from_file : string -> t
end

module String_reader : sig
  include S
  val make : string -> int -> int -> t
  val from_string : string -> t
  val from_io_reader : IO_reader.t -> t
  val from_io : IO.input -> t
  val close : t -> unit
end
