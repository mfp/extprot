
type low_level_type =
    Vint
  | Bits32
  | Bits64
  | Bytes
  | Tuple
  | Htuple

let string_of_low_level_type = function
    Vint -> "Vint"
  | Bits32 -> "Bits32"
  | Bits64 -> "Bits64"
  | Bytes -> "Bytes"
  | Tuple -> "Tuple"
  | Htuple -> "Htuple"

