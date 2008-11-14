exception Bad_digest

module M : sig
  type t
  val from_string : string -> t
  val to_string : t -> string
  val random : unit -> t
  val bad_digest : t
end =
struct
  open Extprot.Random_gen

  type t = string

  let bad_digest = ""

  let from_string x =
    if x = bad_digest then raise Bad_digest;
    x

  let to_string x = x

  let random () =
    let len =
      rand_integer 20 >>= fun n -> return (n + 1)
    in run (rand_string len)
end

include M
