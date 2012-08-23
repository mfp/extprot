
type limits =
    {
      max_elements : int;
      max_message_length : int;
      max_string_length : int;
    }

let default =
  { max_elements = Sys.max_array_length;
    max_message_length = Sys.max_string_length;
    max_string_length = Sys.max_string_length;
  }

let limits = ref default

let get_limits () = !limits
let set_limits x = limits := x

let check_message_length ?message ?constructor ?field n =
  if n > !limits.max_message_length then
    Error.limit_exceeded ?message ?constructor ?field (Error.Message_length n)

let check_string_length ?message ?constructor ?field n =
  if n > !limits.max_string_length then
    Error.limit_exceeded ?message ?constructor ?field (Error.String_length n)

let check_num_elements ?message ?constructor ?field n =
  if n > !limits.max_elements then
    Error.limit_exceeded ?message ?constructor ?field (Error.Number_of_elements n)
