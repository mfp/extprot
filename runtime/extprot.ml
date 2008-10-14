type format_error =
    Bad_field of string * string
  | Bad_message_type
  | Unknown_message_tag of int
  | Unknown_field_tag of int

type extprot_error =
    Missing_element of string * string * string * int
  | Missing_field of string * string * string
  | Bad_format of string * format_error

exception Extprot_error of extprot_error * string

(* let extprot_error err fmt = kprintf (fun s -> raise (Extprot_error (err, s))) fmt *)
let extprot_error err fmt =
  kprintf (fun s -> raise (Extprot_error (err, s)); if true then assert false) fmt

let missing_element msg constr field elm =
  extprot_error (Missing_element (msg, constr, field, elm))
    "Missing element in message %S, constructor %S, field %S, element number %d."
    msg constr field elm

let missing_field msg constr field =
  extprot_error (Missing_field (msg, constr, field))
    "Missing %S field in message %S, constructor %S." field msg constr

let bad_field_format msg constr field =
  extprot_error (Bad_format (msg, (Bad_field (constr, field))))
    "Bad field format for message %S, constructor %S, field %S."
    msg constr field

let unknown_field_tag msg constr field tag =
  extprot_error (Bad_format (msg, (Unknown_field_tag (constr, field, tag))))
    "Unknown tag %d for message %S, constructor %S, field %S."
    tag msg constr field

let bad_message_type msg =
  extprot_error (Bad_format (Bad_message_type ty)) "Bad message type"

let unknown_message_tag msg tag =
  extprot_error (Bad_format (Unknown_message_tag tag))
    "Bad message tag %d" tag
