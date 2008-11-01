
type low_level_type =
    Vint
  | Bits8
  | Bits32
  | Bits64_long
  | Bits64_float
  | Enum
  | Tuple
  | Bytes
  | Htuple
  | Assoc
  | Invalid_ll_type

let string_of_low_level_type = function
    Vint -> "Vint"
  | Bits8 -> "Bits8"
  | Bits32 -> "Bits32"
  | Bits64_long -> "Bits64_long"
  | Bits64_float -> "Bits64_float"
  | Enum -> "Enum"
  | Bytes -> "Bytes"
  | Tuple -> "Tuple"
  | Htuple -> "Htuple"
  | Assoc -> "Assoc"
  | Invalid_ll_type -> failwith "string_of_low_level_type: Invalid_ll_type"

