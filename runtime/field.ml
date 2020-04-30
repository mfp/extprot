
module type S =
sig
  type 'a t

  val from_val : 'a -> 'a t
  val from_fun : Reader.String_reader.t -> (Reader.String_reader.t -> 'a) -> 'a t
  val from_thunk : (unit -> 'a) -> 'a t

  val is_val     : 'a t -> bool
  val force      : 'a t -> 'a
  val discard_packed : 'a t -> unit
  val get_reader : 'a t -> Reader.String_reader.t option
end

module Fast_write =
struct
  type 'a t =
    {
      mutable s : Reader.String_reader.t option;
      mutable v : 'a memo;
    }

  and 'a memo =
    | Delayed of Reader.String_reader.t * (Reader.String_reader.t -> 'a)
    | Value of 'a

  let from_val v = { s = None; v = Value v }

  let from_fun s f = { s = Some s; v = Delayed (s, f) }

  let dummy_reader = Reader.String_reader.from_string ""

  let from_thunk f =
    { s = None; v = Delayed (dummy_reader, (fun _ -> f ())) }

  let force t = match t.v with
    | Value x -> x
    | Delayed (s, f) ->
        let x = f s in
          t.v <- Value x;
          x

  let discard_packed t = match t.v with
    | Value _ -> t.s <- None
    | _ -> ()

  let is_val t = match t.v with
    | Value _ -> true
    | Delayed _ -> false

  let get_reader t = t.s
end

module Discard_data =
struct
  include Fast_write

  let force t = match t.v with
    | Value x -> x
    | Delayed (s, f) ->
        let x = f s in
          t.s <- None;
          t.v <- Value x;
          x
end

module Keep_packed =
struct
  include Fast_write

  let force t = match t.v with
    | Value x -> x
    | Delayed (s, f) -> f s
end

module Keep_compressed(C : sig
    val compress : Reader.String_reader.t -> Reader.String_reader.t
    val decompress : Reader.String_reader.t -> Reader.String_reader.t
  end) =
struct
  include Fast_write

  let from_val v = { s = None; v = Value v }

  let from_fun s f =
    let s = C.compress s in

    let f s =
      let s = C.decompress s in
        f s
    in
      { s = Some s; v = Delayed (s, f); }

  let force t = match t.v with
    | Value x -> x
    | Delayed (s, f) -> f s

  let get_reader t = match t.v with
    | Value _ -> None
    | Delayed (s, _) -> Some (C.decompress s)
end

include Fast_write
