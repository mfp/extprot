(** Buffer pools. *)

(** [make_buffer_pool ~min_size ~max_size make capacity]
  * returns two functions [`Round_up f] and [`Exact g] that are invoked as
  * in [f wanted_size] and return a buffer of the wanted size and a function
  * to release the buffer and return it to the pool. These functions do not
  * block and will return a buffer immediately.
  *
  * [f] will round up the size to a power of two. [g] will not, and will thus
  * allocate fresh buffers if invoked with sizes that are not powers of two.
  *
  * [make_buffer_pool] is used as follows:
  *
  * {[
  *   let `Round_up get_buf, `Exact get_buf_exact =
  *     make_buffer_pool ~min_size ~max_size allocate_buffer
  *     (fun n -> how_many_buffers_to_keep_at_most n)
  *
  *   ...
  *   let b, release = get_buf 4096 in
  *   ...
  *
  *   release ()
  * ]}
  *
  * The release function can be invoked any number of times (the second and
  * later calls are NOPs). If not invoked, the buffer will not be returned to
  * the pool, but this is usually harmless (it only means there will be more
  * allocation later in time).
  *
  * The returned buffers must not be used after they have been released, as
  * they might have been reused at that point.
  *
  * @param min_size buffers of size under [min_size] are not pooled (and
  * always freshly allocated with [make size] (default: 64)
  *
  * @param max_size buffers of size over [max_size] are not pooled (and
  * always freshly allocated with [make size] (default: 65536)
  *
  * @param capacity is used to compute how many buffers to retain at most (as a
  * function of the corresponding size). If [capacity n] returns [m], that
  * means that the the pool will hold at most [m] buffers. Note that buffers
  * are allocated lazily (only when the pool is empty), and the actual number
  * of buffers (of a given size) allocated depends on how many are used
  * simultaneously (before being released).
  * *)
val make_buffer_pool :
  min_size:int ->
  max_size:int ->
  (int -> 'a) ->
  (int -> int) ->
  [`Round_up of int -> 'a * (unit -> unit) ] *
  [`Exact of int -> 'a * (unit -> unit) ]
