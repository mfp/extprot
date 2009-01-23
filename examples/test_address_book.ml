
open Extprot
open Address_book
open Printf

let names = 
  [| "John"; "Jacob"; "Michael"; "Ethan"; "Joshua"; 
     "Daniel"; "Christopher"; "Anthony"; "William"; "Matthew"; |]

let surnames = 
  [| "Smith"; "Johnson"; "Williams"; "Brown"; "Jones"; 
     "Miller"; "Davis"; "García"; "Rodríguez"; "Wilson" |]

let phone_type = [|Phone_type.Mobile; Phone_type.Home; Phone_type.Work|]

let pick a = a.(Random.int (Array.length a))

let random_name () = (pick names, pick surnames)
let name_to_s (n, s) = n ^ " " ^ s
let random_email (n, s) =
  if Random.bool () then Optional.Unset
  else Optional.Set (n ^ string_of_int (Random.int 100) ^ "@" ^ s ^ ".name")
let random_phone () = 
  (sprintf "%d-%d" (Random.int 1000) (Random.int 10000), pick phone_type)
let random_list ?(max_len = 10) f =
  Array.to_list (Array.init (Random.int max_len) (fun _ -> f ()))

let random_person () =
  let n = random_name () in
    { Person.name = name_to_s n; id = Random.int 100000; 
      email = random_email n; phones = random_list random_phone }

let address_book len = 
  {
    Address_book.persons = random_list ~max_len:len random_person
  }

let size = 100000

let () =
  let a = address_book size in
  let t0 = Unix.gettimeofday () in
  let s = Conv.serialize Address_book.write_address_book a in
    eprintf "Serialized %d records in %8.5fs\n" size (Unix.gettimeofday () -. t0);
    output_string stdout s


