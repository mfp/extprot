open Printf
module C = Test_types.Complex_rtt
module M = Extprot.Msg_buffer

let make_array n = Array.init n (fun _ -> Gen_data.generate Gen_data.complex_rtt)
let dec = C.read_complex_rtt
let dec_io = C.io_read_complex_rtt
let enc = C.write_complex_rtt

let len = ref 50000

let arg_spec =
  Arg.align
    [
      "-n", Arg.Set_int len, "Number of structures.";
    ]

let time msg f x =
  let t0 = Unix.gettimeofday () in
  let y = f x in
  let dt = Unix.gettimeofday () -. t0 in
    printf "[%s] %8.5fs\n%!" msg dt;
    y

let bm_wr_rd msg write open_rd read a =
  Gc.compact ();
  printf "==== %s ====\n" msg;
  let out = time "write" write a in
    Gc.compact ();
    time "read" read (open_rd out)

let () =
  Arg.parse arg_spec ignore "Usage:";
  let a = time (sprintf "gen array of length %d" !len) make_array !len in
  let pr_size s = printf "** Serialized in %d bytes.\n%!" (String.length s) in
  let bm_extprot msg open_rd read x =
    bm_wr_rd ("extprot " ^ msg)
      (fun a -> let b = M.create () in Array.iter (enc b) a; b) open_rd read x
  in

    bm_extprot "String_reader"
      (fun b ->
         let s = M.contents b in
           pr_size s;
           Extprot.Reader.String_reader.make s 0 (String.length s))
      (fun rd -> try while true do ignore (dec rd) done with End_of_file -> ())
      a;

    bm_extprot "IO_reader"
      (fun b ->
         let s = M.contents b in
           pr_size s;
           IO.input_string s)
      (fun rd -> try while true do ignore (dec_io rd) done with End_of_file -> ())
      a;

    bm_wr_rd "Marshal"
      (fun a -> Marshal.to_string a [])
      (fun s -> pr_size s; s)
      (fun s -> ignore (Marshal.from_string s 0))
      a
