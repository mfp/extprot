open Printf

type field_format_error =
    Bad_field_format
  | Bad_field_tag

type message_error =
    Bad_message_type
  | Bad_message_tag

type format_error =
    Field_error of string * string * string * field_format_error
  | Message_error of string * message_error
  | Unknown_error

type extprot_error =
    Missing_element of string * string * string * int
  | Missing_field of string * string * string
  | Bad_format of format_error

exception Extprot_error of extprot_error * string

let extprot_error err fmt =
  (* kprintf (fun s -> if true then raise (Extprot_error (err, s))) fmt *)
  kprintf (fun s -> raise (Extprot_error (err, s))) fmt

let missing_element msg constr field elm =
  extprot_error (Missing_element (msg, constr, field, elm))
    "Missing element in message %S, constructor %S, field %S, element number %d."
    msg constr field elm

let missing_field msg constr field =
  extprot_error (Missing_field (msg, constr, field))
    "Missing %S field in message %S, constructor %S." field msg constr

let bad_field_format msg constr field =
  extprot_error (Bad_format (Field_error (msg, constr, field, Bad_field_format)))
    "Bad field format for message %S, constructor %S, field %S."
    msg constr field

let unknown_field_tag msg constr field tag =
  extprot_error (Bad_format (Field_error (msg, constr, field, Bad_field_tag)))
    "Unknown tag %d for message %S, constructor %S, field %S."
    tag msg constr field

let bad_message_type msg =
  extprot_error (Bad_format (Message_error (msg, Bad_message_type))) "Bad message type"

let unknown_message_tag msg tag =
  extprot_error (Bad_format (Message_error (msg, Bad_message_tag)))
    "Bad message tag %d" tag

let bad_format () =
  extprot_error (Bad_format Unknown_error)
    "Unexpected or unknown type"

