module M : sig
  type t 
  val from_string : string -> t
  val to_string : t -> string
  val random : unit -> t
end =
struct
  open Extprot.Random_gen

  type t = string
  let from_string x = x
  let to_string x = x

  let random () = run (rand_string (rand_integer 20))
end

include M
