module M : sig
  type t 
  val from_string : string -> t
  val to_string : t -> string
end =
struct
  type t = string
  let from_string x = x
  let to_string x = x
end

include M
