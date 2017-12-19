open Printf
open ExtArray
module C = Test_types.Complex_rtt
module M = Extprot.Msg_buffer
module E = Extprot
module B = Buffer

let dec = C.read_complex_rtt
let dec_io = C.io_read_complex_rtt
let dec_io_fast = C.fast_io_read_complex_rtt
let enc = C.write_complex_rtt

let generate () =
  Gen_data.generate ~state:(Random.State.make [|42|]) Gen_data.complex_rtt

let make_array generate n =
  let maxl = 10000 in
  let a = Array.init (min maxl n) (fun _ -> generate ()) in match n with
      n when n <= maxl -> a
    | n ->
        let rem = Array.sub a 0 (n mod maxl) in
          Array.concat (rem :: Array.to_list (Array.make (n / maxl) a))

let len = ref 50000
let in_file = ref None
let out_file = ref None
let rounds = ref 2
let dump = ref `No


let () = Random.self_init ()
let seed = ref (Random.int 0x3fffffff)

let arg_spec =
  Arg.align
    [
      "-n", Arg.Set_int len, "INT Number of structures.";
      "-i", Arg.String (fun s -> in_file := Some s), "FILE Input data from specified file.";
      "-o", Arg.String (fun s -> out_file := Some s), "FILE Output data to specified file.";
      "-r", Arg.Set_int rounds, "INT Number of iterations for (de)serialization.";
      "--seed", Arg.Set_int seed, "INT Seed for random generator.";
      "--dump", Arg.Unit (fun () -> dump := `PP), " Dump data in readable form to stdout.";
      "--dumpbin", Arg.Unit (fun () -> dump := `Binary), " Dump data in encoded form to stdout.";
      "--xml", Arg.Unit (fun () -> dump := `Xml), " Dump data in XML form to stdout.";
    ]

let time f x =
  let t0 = Unix.gettimeofday () in
  let y = f x in
  let dt = Unix.gettimeofday () -. t0 in
    (y, dt)

let bm msg f x =
  let y, dt = time f x in
    eprintf "[%s]   %8.5fs\n%!" msg dt;
    y

let report_with_avg label dts =
  eprintf "[%-8s]    min: %8.5fs   avg: %8.5fs\n"
    label
    (Array.fold_left min max_float dts)
    (Array.fold_left (+.) 0. dts /. float !rounds)

let bm_wr_rd msg write open_rd read a =
  eprintf "\n==== %s ====\n" msg;
  let dts1 = Array.init !rounds (fun _ -> snd (time (write ~report:false) a)) in
  let out  = write ~report:true a in
  let dts2 = Array.init !rounds (fun  _ -> snd (time read (open_rd out))) in
    report_with_avg "write" dts1;
    report_with_avg "read" dts2

let pr_size n = eprintf "** Serialized in %d bytes.\n%!" n

let encode_to_msg_buf ~report x =
  let b = M.create () in
    Array.iter (enc b) x;
    if report then pr_size (M.length b);
    b

let bm_extprot msg open_rd read x =
  bm_wr_rd ("extprot " ^ msg) encode_to_msg_buf open_rd read x

let benchmarks =
  [
    "string_reader", begin fun a ->
      bm_extprot "String_reader"
        (fun b -> E.Reader.String_reader.from_string (M.contents b))
        (fun rd -> try while true do ignore (dec rd) done with End_of_file -> ())
        a
    end;

    "io_reader", begin fun a ->
      bm_extprot "IO_reader"
        (fun b -> E.Reader.IO_reader.from_string (M.contents b))
        (fun rd -> try while true do ignore (dec_io rd) done with End_of_file -> ())
        a
    end;

    "io_reader (fast)", begin fun a ->
      bm_extprot "IO_reader (buffered)"
        (fun b -> E.Reader.IO_reader.from_string (M.contents b))
        (fun rd -> try while true do ignore (dec_io_fast rd) done with End_of_file -> ())
        a
    end;

    "marshal_array", begin fun a ->
      bm_wr_rd "Marshal (array)"
        (fun ~report a ->
           let s = Marshal.to_string a [Marshal.No_sharing] in
             if report then pr_size (String.length s);
             s)
        (fun s -> s)
        (fun s -> ignore (Marshal.from_string s 0))
        a;
    end;

    "marshal", begin fun a ->
      bm_wr_rd "Marshal (individual msgs)"
        (fun ~report a ->
           let b = B.create 256 in
             Array.iter (fun x -> B.add_string b (Marshal.to_string x [])) a;
             if report then pr_size (B.length b);
             B.contents b)
        (fun s -> s)
        (fun s ->
           let s   = Bytes.unsafe_of_string s in
           let max = Bytes.length s in
           let rec loop n =
             if n < max then begin
               let len = Marshal.total_size s n in
                 ignore (Marshal.from_bytes s n);
                 loop (n + len)
             end
           in loop 0)
        a
    end;
  ]

let all_benchmarks = List.sort String.compare (List.map fst benchmarks)
let bms_desc = String.concat ", " all_benchmarks
let bms = ref None

let help_msg =
  "Usage: bm_01 [options] [benchmarks]\n\n\
   Known benchmarks: " ^ bms_desc ^ "\n"

let select_bm bm =
  if not (List.mem_assoc bm benchmarks) then begin
    eprintf "Unknown benchmark %S.\n\n" bm;
    Arg.usage arg_spec help_msg;
    exit (-1)
  end;
  match !bms with
      None -> bms := Some [bm]
    | Some l -> bms := Some (l @ [bm])

let mk_garbage size =
  let h = Hashtbl.create size in
    for i = 0 to size do
      Hashtbl.add h i i
    done;
    h

(* This garbage is allocated ensure the major heap is not empty so that the
 * scan+mark cost is included in the benchmark. *)
let g = mk_garbage 1_000_000

let main () =
  Arg.parse arg_spec select_bm help_msg;

  Random.init !seed;

  let a = match !in_file with
      None -> bm (sprintf "gen array of length %d" !len) (make_array generate) !len
    | Some f ->
        let io = E.Reader.IO_reader.from_file f in
        let rec loop l =
          match try Some (dec_io io) with End_of_file -> None with
              None -> Array.of_list (List.rev l)
            | Some t -> loop (t :: l)
        in loop [] in

  let run_bm name =
    Gc.compact ();
    List.assoc name benchmarks a
  in

    Option.may
      (fun f ->
         let io = IO.output_channel (open_out f) in
           IO.nwrite io @@ Bytes.unsafe_of_string @@ M.contents @@ encode_to_msg_buf ~report:true a;
           IO.close_out io)
      !out_file;

    List.iter run_bm (Option.default all_benchmarks !bms);

    match !dump with
        `No -> ()
      | `Binary -> print_string (M.contents (encode_to_msg_buf ~report:true a))
      | `PP -> Array.iter (Format.printf "%a@?\n" C.pp_complex_rtt) a
      | `Xml ->
          let b = Buffer.create 256 in
            Array.iter
              (fun x -> Gen_data.Xml.complex_rtt_to_xml x b;
                        Buffer.add_char b '\n')
              a;
            print_endline (Buffer.contents b)

let () =
  try
    main ()
  with E.Error.Extprot_error (e, loc) ->
    Format.eprintf "Extprot_error: %a\n" E.Error.pp_extprot_error (e, loc)
