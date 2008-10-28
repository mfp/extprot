
type low_level_type =
    Vint
  | Bits8
  | Bits32
  | Bits64_long
  | Bits64_float
  | Tuple
  | Bytes
  | Htuple
  | Invalid_ll_type

let string_of_low_level_type = function
    Vint -> "Vint"
  | Bits8 -> "Bits8"
  | Bits32 -> "Bits32"
  | Bits64_long -> "Bits64_long"
  | Bits64_float -> "Bits64_float"
  | Bytes -> "Bytes"
  | Tuple -> "Tuple"
  | Htuple -> "Htuple"
  | Invalid_ll_type -> failwith "string_of_low_level_type: Invalid_ll_type"

