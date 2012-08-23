(** Runtime limits imposed on messages being read.
  *
  * If they are exceeded, a [Extprot.Error.Limit_exceeded] error will be
  * raised.
  * *)

(** Constraints verified when a message is read. *)
type limits = {
  (** Maximum number of elements allowed in a message, a list or an array. *)
  max_elements : int;

  (** Maximum allowed message length. *)
  max_message_length : int;

  (** Maximum allowed string length. *)
  max_string_length : int;
}

(** Default limits. Uses [Sys.max_string_length] as [max_message_length] and
  * [max_string_length], and [Sys.max_elements] as [max_elements]. *)
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
