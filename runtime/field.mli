
module type S =
sig
  type 'a t
  type hint
  type path

  module Hint_path :
  sig
    type t = path

    val null : path

    val append_type   : t -> string -> t
    val append_constr : t -> string -> int -> t
    val append_field  : t -> string -> int -> t
  end

  val from_val : 'a -> 'a t
  val from_fun : ?hint:hint -> level:int -> path:path -> Reader.String_reader.t -> (Reader.String_reader.t -> 'a) -> 'a t
  val from_thunk : (unit -> 'a) -> 'a t

  val is_val     : 'a t -> bool
  val force      : 'a t -> 'a
  val discard_packed : 'a t -> unit
  val get_reader : 'a t -> Reader.String_reader.t option
end

(** Field implementation that retains serialized data to support fast
 * serialization. *)
module Fast_write   : S

(** Field implementation that discards serialized data when the value is
 * forced. This reduces memory usage (since the underlying buffer can be
 * released) but will make serialization slower.
 * *)
module Discard_data : S

(** Field implementation that keeps data in "packed" (serialized) form.
 * This minimizes memory usage, but makes both access and serialization
 * slower.
 * *)
module Keep_packed  : S

(** Field implementation that keeps data in compressed (serialized and then
 * compressed) form.
 * This minimizes memory usage, but makes both access and serialization
 * slower.
 * *)
module Keep_compressed :
  functor (C : sig
    val compress   : Reader.String_reader.t -> Reader.String_reader.t
    val decompress : Reader.String_reader.t -> Reader.String_reader.t
  end) -> S

(** The functionality in this module defaults to {!Fast_write}. *)
include S
