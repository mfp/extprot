
type limits =
    {
      is_num_elements_ok : int -> bool;
      is_message_length_ok : int -> bool;
      is_string_length_ok : int -> bool;
    }

let default =
  { is_num_elements_ok = (fun n -> n < Sys.max_array_length);
    is_message_length_ok = (fun n -> n < Sys.max_string_length);
    is_string_length_ok = (fun n -> n < Sys.max_string_length);
  }

let limits = ref default

let get_limits () = !limits
let set_limits x = limits := x

let check_message_length ?message ?constructor ?field n =
  if not (!limits.is_message_length_ok n) then
    Error.limit_exceeded ?message ?constructor ?field (Error.Message_length n)

let check_string_length ?message ?constructor ?field n =
  if not (!limits.is_string_length_ok n) then
    Error.limit_exceeded ?message ?constructor ?field (Error.String_length n)

let check_num_elements ?message ?constructor ?field n =
  if not (!limits.is_num_elements_ok n) then
    Error.limit_exceeded ?message ?constructor ?field (Error.Number_of_elements n)
