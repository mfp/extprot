
type low_level_type =
    Vint
  | Vint_pos
  | Bits32
  | Bits64_long
  | Bits64_float
  | Bytes
  | Tuple
  | Htuple

let string_of_low_level_type = function
    Vint -> "Vint"
  | Vint_pos -> "Vint_pos"
  | Bits32 -> "Bits32"
  | Bits64_long -> "Bits64_long"
  | Bits64_float -> "Bits64_float"
  | Bytes -> "Bytes"
  | Tuple -> "Tuple"
  | Htuple -> "Htuple"

