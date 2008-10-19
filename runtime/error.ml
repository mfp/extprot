open Printf
module PP = Pretty_print

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

let pp_field_format_error pp = function
    Bad_field_format -> PP.fprintf pp "Bad_field_format"
  | Bad_field_tag -> PP.fprintf pp "Bad_field_tag"

let pp_message_format_error pp = function
  | Bad_message_type -> PP.fprintf pp "Bad_message_type"
  | Bad_message_tag -> PP.fprintf pp "Bad_message_tag"

let pp_format_error pp = function
    Field_error (msg, constr, field, field_format_error) ->
      PP.pp_tuple4 ~constr:"Field_error"
        PP.pp_string PP.pp_string PP.pp_string pp_field_format_error
        pp (msg, constr, field, field_format_error)
  | Message_error (s, msg_error) ->
      PP.pp_tuple2 ~constr:"Message_error"
        PP.pp_string pp_message_format_error
        pp (s, msg_error)
  | Unknown_error -> PP.fprintf pp "Unknown_error"

let pp_extprot_error pp = function
    Missing_element (message, constr, field, elmno) ->
      PP.pp_tuple4 ~constr:"Missing_element"
        PP.pp_string PP.pp_string PP.pp_string PP.pp_int
        pp (message, constr, field, elmno)
  (* | Missing_field (message, constr, field) -> *)
  | Missing_field (message, constr, field) ->
      PP.pp_tuple3 ~constr:"Missing_field"
        PP.pp_string PP.pp_string PP.pp_string
        pp (message, constr, field)
  | Bad_format err ->
      PP.fprintf pp "Bad_format %a" pp_format_error err

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

