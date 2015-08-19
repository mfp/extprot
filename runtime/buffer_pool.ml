
module POOL :
sig
  type 'a t

  val make : (unit -> 'a) -> int -> 'a t
  val take : 'a t -> 'a
  val give_back : 'a t -> 'a -> unit
end =
struct
  type 'a t =
      {
        create   : unit -> 'a;
        q        : 'a Stack.t;
        capacity : int;
        mutable q_size : int;
      }

  let make create capacity =
    { create; capacity;
      q_size = 0;
      q = Stack.create ();
    }

  let take t =
    if Stack.is_empty t.q then
       t.create ()
    else
      let x = Stack.pop t.q in
        t.q_size <- t.q_size - 1;
        x

  let give_back t x =
    if t.q_size < t.capacity then begin
      Stack.push x t.q;
      t.q_size <- t.q_size + 1;
    end
end

let round_to_pow2 n =
  let m = ref 1 in
    while !m < n do
      m := !m * 2;
    done;
    !m

let is_pow2 n =
  let m = n lor (n lsr 1) in
  let m = m lor (m lsr 2) in
  let m = m lor (m lsr 4) in
  let m = m lor (m lsr 8) in
  let m = m lor (m lsr 16) in
    n land (m lsr 1) = 0



let make_buffer_pool ~min_size ~max_size create capacity =
  let h = Hashtbl.create 13 in

  let get_pool size =
    try
      Hashtbl.find h size
    with Not_found ->
      let p = POOL.make (fun () -> create size) (capacity size) in
        Hashtbl.add h size p;
        p in

  let is_interesting size = is_pow2 size in

  let get ~exact size =
    if (exact && not (is_interesting size)) || size < min_size || size > max_size then
      (create size, (fun () -> ()))
    else
      let p = get_pool (round_to_pow2 size) in
      let x = POOL.take p in

      let released = ref false in

      let release () =
        if not !released then begin
          released := true;
          POOL.give_back p x
        end
      in
        (x, release)
  in
    (`Round_up (get ~exact:false), `Exact (get ~exact:true))
