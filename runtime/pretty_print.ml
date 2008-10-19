open Format

let fprintf = Format.fprintf

let pp f x =
  let b = Buffer.create 16 in
  let pp = formatter_of_buffer b in
    fprintf pp "%a@?" f x;
    Buffer.contents b;;

let ppfmt fmt =
  let b = Buffer.create 16 in
    kfprintf
      (fun pp -> pp_print_flush pp (); Buffer.contents b)
      (formatter_of_buffer b)
      fmt

let rec fprintfs sep f pp = function
    [] -> ()
  | [x] -> fprintf pp "%a" f x
  | x::l -> fprintf pp "%a%a%a" f x sep () (fprintfs sep f) l

let pp_list f pp l =
  let pr_elems = fprintfs (fun pp () -> fprintf pp ";@ ") f in
    fprintf pp "[@[<1>@ %a@ @]]" pr_elems l

let pp_array f pp l =
  let pr_elems = fprintfs (fun pp () -> fprintf pp ";@ ") f in
    fprintf pp "[|@[<1>@ %a@ @]|]" pr_elems (Array.to_list l)

let pp_struct fields pp t =
  let rec pr_fields pp = function
      [] -> ()
    | [name, f] -> fprintf pp "@[<1>%s@ =@ %a@]" name f t
    | (name, f)::l -> fprintf pp "@[<1>%s@ =@ %a@];@ %a" name f t pr_fields l
  in
    fprintf pp "{@[<2>@ %a @]}" pr_fields fields

let pp_tuple2 f1 f2 pp (a, b) = fprintf pp "(@[<1>%a,@ %a@])" f1 a f2 b

let pp_tuple3 f1 f2 f3 pp (a, b,c) =
  fprintf pp "(@[<1>%a,@ %a,@ %a@])" f1 a f2 b f3 c

let pp_tuple4 f1 f2 f3 f4 pp (a, b, c, d) =
  fprintf pp "(@[<1>%a,@ %a,@ %a,@ %a@])" f1 a f2 b f3 c f4 d

let pp_tuple5 f1 f2 f3 f4 f5 pp (a, b, c, d, e) =
  fprintf pp "(@[<1>%a,@ %a,@ %a,@ %a,@ %a@])" f1 a f2 b f3 c f4 d f5 e

let pp_tuple6 f1 f2 f3 f4 f5 f6 pp (a, b, c, d, e, f) =
  fprintf pp "(@[<1>%a,@ %a,@ %a,@ %a,@ %a,@ %a@])"
    f1 a f2 b f3 c f4 d f5 e f6 f

let pp_field fieldf f pp t = f pp (fieldf t)

let pp_string pp = fprintf pp "%S"

let pp_int pp = function
    n when n < 0 -> fprintf pp "(%d)" n
  | n -> pp_print_int pp n

let pp_bool = pp_print_bool
let pp_char pp = fprintf pp "%C"
let pp_float = pp_print_float
let pp_int64 pp n = fprintf pp "%s" (Int64.to_string n)
