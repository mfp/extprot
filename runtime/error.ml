module PP = Pretty_print

type location =
    Null_location
  | Field of string * location
  | Message of string * string option * location

type format_error =
    Bad_wire_type of Types.low_level_type option
  | Unknown_tag of int
  | Conversion_error of exn

type extprot_error =
    Missing_tuple_element of int
  | Missing_field
  | Bad_format of format_error
  | Limit_exceeded of limit

and limit =
    Message_length of int
  | Number_of_elements of int
  | String_length of int

exception Extprot_error of extprot_error * location

let rec pp_location pp = function
  | Null_location -> PP.fprintf pp "()"
  | Field (field, loc) ->
      PP.fprintf pp "@[<1>%s.@,%a@]" field pp_location loc
  | Message (msg, constr, loc) -> match constr with
        None -> PP.fprintf pp "@[<1>%s.@,%a@]" (String.capitalize_ascii msg) pp_location loc
      | Some c -> PP.fprintf pp "@[<1>%s_%s.@,%a@]"
                    (String.capitalize_ascii msg) (String.capitalize_ascii c) pp_location loc

let pp_format_error pp = function
  | Bad_wire_type ty ->
      PP.fprintf pp
        "Bad_wire_type %a"
        (PP.pp_option
           (fun pp ty -> PP.fprintf pp "%s" (Types.string_of_low_level_type ty)))
        ty
  | Unknown_tag n -> PP.fprintf pp "Unknown_tag %d" n
  | Conversion_error exn ->
      PP.fprintf pp "Conversion_error (%s)" (Printexc.to_string exn)

let pp_limit pp = function
    Message_length n -> PP.fprintf pp "Message_length %d" n
  | Number_of_elements n -> PP.fprintf pp "Number_of_elements %d" n
  | String_length n -> PP.fprintf pp "String_length %d" n

let pp_extprot_error pp (e, loc) = match e with
    Missing_tuple_element elmno ->
      PP.pp_tuple2 ~constr:"Missing_tuple_element" PP.pp_int pp_location
        pp (elmno, loc)
  | Missing_field -> PP.fprintf pp "@[<1>Missing_field@ (%a)@]" pp_location loc
  | Bad_format err ->
      PP.pp_tuple2 ~constr:"Bad_format" pp_format_error pp_location pp (err, loc)
  | Limit_exceeded lim ->
      PP.fprintf pp "@[<1>Limit_exceeded@ %a@]" pp_limit lim

let () =
  Printexc.register_printer
    (function
         Extprot_error (err, loc) ->
           Some (PP.ppfmt "Extprot_error %a" pp_extprot_error (err, loc))
       | _ -> None)

let extprot_error err loc = raise (Extprot_error (err, loc))

let bad_format err loc = extprot_error (Bad_format err) loc

let location ~message ~constructor ~field ?(loc = Null_location) () =
  let loc = match field with None -> loc | Some f -> Field (f, loc) in
    match (message, constructor) with
        None, None -> loc
      | Some m, cons -> Message (m, cons, loc)
      | None, cons -> Message ("<unknown>", cons, loc)

let failwith_location ?message ?constructor ?field err loc =
  extprot_error err (location ~message ~constructor ~field ~loc ())

let missing_tuple_element ?message ?constructor ?field elm =
  extprot_error (Missing_tuple_element elm) (location ~message ~constructor ~field ())

let missing_field ?message ?constructor ?field () =
  extprot_error Missing_field (location ~message ~constructor ~field ())

let bad_wire_type ?message ?constructor ?field ?ll_type () =
  bad_format (Bad_wire_type ll_type) (location ~message ~constructor ~field ())

let unknown_tag ?message ?constructor ?field tag =
  bad_format (Unknown_tag tag) (location ~message ~constructor ~field ())

let conversion_error ?message ?constructor ?field exn =
  bad_format (Conversion_error exn) (location ~message ~constructor ~field ())

let limit_exceeded ?message ?constructor ?field lim =
  extprot_error (Limit_exceeded lim) (location ~message ~constructor ~field ())
