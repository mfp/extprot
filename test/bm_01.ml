open Printf
open ExtArray
module C = Test_types.Complex_rtt
module M = Extprot.Msg_buffer
module E = Extprot
module B = Buffer

let make_array n = Array.init n (fun _ -> Gen_data.generate Gen_data.complex_rtt)
let dec = C.read_complex_rtt
let dec_io = C.io_read_complex_rtt
let enc = C.write_complex_rtt

let len = ref 50000
let in_file = ref None
let out_file = ref None
let rounds = ref 2

let arg_spec =
  Arg.align
    [
      "-n", Arg.Set_int len, "INT Number of structures.";
      "-i", Arg.String (fun s -> in_file := Some s), "FILE Input data from specified file.";
      "-o", Arg.String (fun s -> out_file := Some s), "FILE Output data to specified file.";
      "-r", Arg.Set_int rounds, "INT Number of iterations for deserialization.";
    ]

let time f x =
  let t0 = Unix.gettimeofday () in
  let y = f x in
  let dt = Unix.gettimeofday () -. t0 in
    (y, dt)

let bm msg f x =
  let y, dt = time f x in
    printf "[%s] %8.5fs\n%!" msg dt;
    y

let bms = ref `All

let bm_wr_rd id msg write open_rd read a =
    let run () =
      Gc.compact ();
      printf "==== %s ====\n" msg;
      let out = bm "write" write a in
      let dt = ref 0. in
        for i = 0 to !rounds do
          Gc.compact ();
          let _, ddt = time read (open_rd out) in
            dt := !dt +. ddt
        done;
        printf "[read] %8.5fs / iter\n" (!dt /. float !rounds)

    in match !bms with
        `All -> run ()
      | `Some l -> if List.mem id l then run ()

let set_tests id = match !bms with
    `All -> bms := `Some [id]
  | `Some l -> bms := `Some (id :: l)

let main () =
  Arg.parse arg_spec set_tests
    "Usage: bm_01 [options] [benchmarks]\n\n\
     Known benchmarks: string_reader, io_reader, marshal, marshal_array\n";

  let pr_size n = printf "** Serialized in %d bytes.\n%!" n in

  let encode_to_msg_buf x =
    let b = M.create () in
      Array.iter (enc b) x;
      pr_size (M.length b);
      b in

  let bm_extprot id msg open_rd read x =
    bm_wr_rd id ("extprot " ^ msg) encode_to_msg_buf open_rd read x in

  let a = match !in_file with
      None -> bm (sprintf "gen array of length %d" !len) make_array !len
    | Some f ->
        let io = E.Reader.IO_reader.from_file f in
        let rec loop l =
          match try Some (dec_io io) with End_of_file -> None with
              None -> Array.of_list (List.rev l)
            | Some t -> loop (t :: l)
        in loop []
  in
    Option.may
      (fun f ->
         let io = IO.output_channel (open_out f) in
           IO.nwrite io (M.contents (encode_to_msg_buf a));
           IO.close_out io)
      !out_file;

    bm_extprot "string_reader" "String_reader"
      (fun b ->
         let s = M.contents b in
           E.Reader.String_reader.make s 0 (String.length s))
      (fun rd -> try while true do ignore (dec rd) done with End_of_file -> ())
      a;

    bm_extprot "io_reader" "IO_reader"
      (fun b -> E.Reader.IO_reader.from_string (M.contents b))
      (fun rd -> try while true do ignore (dec_io rd) done with End_of_file -> ())
      a;

    bm_wr_rd "marshal" "Marshal (array)"
      (fun a -> let s = Marshal.to_string a [] in pr_size (String.length s); s)
      (fun s -> s)
      (fun s -> ignore (Marshal.from_string s 0))
      a;

    bm_wr_rd "marshal_array" "Marshal (individual msgs)"
      (fun a ->
         let b = B.create 256 in
           Array.iter (fun x -> B.add_string b (Marshal.to_string x [])) a;
           pr_size (B.length b);
           B.contents b)
      (fun s -> s)
      (fun s ->
         let max = String.length s in
         let rec loop n =
           if n < max then begin
             let len = Marshal.total_size s n in
               ignore (Marshal.from_string s n);
               loop (n + len)
           end
         in loop 0)
      a;
    ()

let () =
  try
    main ()
  with E.Error.Extprot_error (e, loc) ->
    Format.printf "Extprot_error: %a\n" E.Error.pp_extprot_error (e, loc)
