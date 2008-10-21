
module type MONAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

let (@@) f x = f x

module RS = Random.State
module R = Random

module type RANDOM_MONAD =
sig
  include MONAD
  val rand_bool : bool t
  val rand_char : char t
  val rand_integer : int -> int t
  val rand_int : int t
  val rand_int64 : Int64.t t
  val rand_float : float t
  val rand_string : int t -> string t
  val rand_list : int t -> 'a t -> 'a list t
  val rand_array : int t -> 'a t -> 'a array t
  val rand_choice : 'a t list -> 'a t
end

module Rand : sig
  include RANDOM_MONAD
  val run : ?state:RS.t -> 'a t -> 'a
end =
struct
  type 'a t = RS.t -> ('a * RS.t)

  let return a = fun s -> (a, s)
  let bind m f = fun s -> let (x, s) = m s in f x s
  let (>>=) = bind

  let random f x = fun s ->
    R.set_state s;
    let r = f x in
      (r, R.get_state ())

  let lift f (x, s) = (f x, s)

  let rand_integer n = random R.int n

  let rand_bool = random R.bool ()

  let rand_char =
    rand_integer 255 >>= fun c ->
    return (Char.chr c)

  let gen_int64 () =
    Int64.sub (R.int64 Int64.max_int) (R.int64 Int64.max_int)

  let rand_int64 = random gen_int64 ()

  let rand_float = random (fun () -> Int64.float_of_bits (gen_int64 ())) ()

  let rand_string len =
    (* TODO: fill string with random stuff *)
    len >>= fun n -> return (String.create n)

  let rand_int =
    random
      (fun () -> Int64.to_int (Int64.shift_right (gen_int64 ()) (8 * R.int 8)))
      ()

  let rand_list len elm =
    len >>= fun len ->
    let rec loop acc = function
        0 -> return acc
      | n -> elm >>= fun e -> loop (e :: acc) (n-1)
    in loop [] len

  let rand_array len elm = rand_list len elm >>= fun l -> return (Array.of_list l)

  let rand_choice l = rand_integer (List.length l) >>= List.nth l

  let run ?(state = R.get_state ()) m = fst (m state)

end

