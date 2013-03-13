open Monad

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
  val rand_tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val rand_tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val rand_tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val rand_tuple5 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd  * 'e) t
  val rand_tuple6 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> ('a * 'b * 'c * 'd  * 'e * 'f) t
  val rand_tuple7 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t ->
    ('a * 'b * 'c * 'd  * 'e * 'f * 'g) t
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
    len >>= fun n ->
      let s = String.create n in
      let rec loop = function
          n when n < 0 -> return s
        | n -> rand_integer 255 >>= fun c -> s.[n] <- Char.chr c; loop (n - 1)
      in loop (n - 1)

  let rand_int =
    random
      (fun () ->
         Int32.to_int
           (Int32.shift_right
              (Int32.sub (R.int32 Int32.max_int) (R.int32 Int32.max_int))
              (1 + 8 * R.int 4)))
      ()

  let rand_list len elm =
    len >>= fun len ->
    let rec loop acc = function
        0 -> return acc
      | n -> elm >>= fun e -> loop (e :: acc) (n-1)
    in loop [] len

  let rand_array len elm = rand_list len elm >>= fun l -> return (Array.of_list l)

  let rand_choice l = rand_integer (List.length l) >>= List.nth l

  let rand_tuple2 a b =
    a >>= fun a -> b >>= fun b -> return (a, b)

  let rand_tuple3 a b c =
    a >>= fun a -> b >>= fun b -> c >>= fun c -> return (a, b, c)

  let rand_tuple4 a b c d =
    a >>= fun a -> b >>= fun b -> c >>= fun c -> d >>= fun d -> return (a, b, c, d)

  let rand_tuple5 a b c d e =
    a >>= fun a -> b >>= fun b ->
    c >>= fun c -> d >>= fun d -> e >>= fun e -> return (a, b, c, d, e)

  let rand_tuple6 a b c d e f =
    a >>= fun a -> b >>= fun b -> c >>= fun c ->
    d >>= fun d -> e >>= fun e -> f >>= fun f -> return (a, b, c, d, e, f)

  let rand_tuple7 a b c d e f g =
    a >>= fun a -> b >>= fun b -> c >>= fun c ->
    d >>= fun d -> e >>= fun e -> f >>= fun f -> g >>= fun g ->
      return (a, b, c, d, e, f, g)

  let run ?(state = R.get_state ()) m = fst (m state)

end

include Rand
