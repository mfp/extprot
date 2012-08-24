(** Runtime limits imposed on messages being read.
  *
  * If they are exceeded, a [Extprot.Error.Limit_exceeded] error will be
  * raised.
  * *)

(** Constraints verified when a message is read. *)
type limits = {
  (** Check number of elements allowed in a message, a list or an array.
    * Return [false] if beyond the limit. *)
  is_num_elements_ok : int -> bool;

  (** Check message length. Return [false] if beyond the limit. *)
  is_message_length_ok : int -> bool;

  (** Check string length. Return [false] if beyond the limit. *)
  is_string_length_ok : int -> bool
}

(** Default limits. They verify that:
  * * both the number of elements in a message and in arrays/lists are
  *   below [Sys.max_array_length],
  * * and that both the message and the string lengths are below
  *   [Sys.max_string_length] *)
val default : limits

(** Return current limits. *)
val get_limits : unit -> limits

(** Set new limits. *)
val set_limits : limits -> unit

(** / **)
val check_message_length :
  ?message:string -> ?constructor:string -> ?field:string -> int -> unit
val check_string_length :
  ?message:string -> ?constructor:string -> ?field:string -> int -> unit
val check_num_elements :
  ?message:string -> ?constructor:string -> ?field:string -> int -> unit
