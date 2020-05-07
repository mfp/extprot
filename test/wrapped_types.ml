
module Make (M : sig type t end) =
struct
  type t = A of M.t

  let from_x x   = A x
  let to_x (A x) = x
end

module Wrap_bool = Make(struct type t = bool end)
module Wrap_int = Make(struct type t = int end)
module Wrap_char = Make(struct type t = char end)
module Wrap_i32 = Make(struct type t = Int32.t end)
module Wrap_i64 = Make(struct type t = Int64.t end)
module Wrap_float = Make(struct type t = float end)
module Wrap_string = Make(struct type t = string end)
module Wrap_tuple_iii = Make(struct type t = int * int * int end)
module Wrap_tuple_isi = Make(struct type t = int * string * int end)

module Sum = struct type t = A | B of string end
module Rec = struct type t = { a : int; b : string } end

module Wrap_sum = Make(struct type t = Sum.t end)
module Wrap_rec = Make(struct type t = Rec.t end)
module Wrap_list = Make(struct type t = int list end)
module Wrap_list2 = Make(struct type t = Rec.t list end)
module Wrap_list3 = Make(struct type t = Sum.t list end)
module Wrap_array = Make(struct type t = int array end)
module Wrap_array2 = Make(struct type t = Rec.t array end)
module Wrap_array3 = Make(struct type t = Sum.t array end)
