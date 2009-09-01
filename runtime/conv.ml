
exception Wrong_protocol_version of int * int (* max_known, found *)

let serialize f x =
  let b = Msg_buffer.create () in
    f b x;
    Msg_buffer.contents b

let deserialize f s = f (Reader.String_reader.from_string s)

let read f io = f (Reader.IO_reader.from_io io)

let get_buf = function
    None -> Msg_buffer.create ()
  | Some b -> Msg_buffer.clear b; b

let write ?buf (f : Msg_buffer.t -> 'a -> unit) io (x : 'a) =
  let buf = get_buf buf in
    f buf x;
    IO.nwrite io (Msg_buffer.contents buf)

let read_versioned fs io =
  let version = IO.read_ui16 io in
    if version < Array.length fs then
      fs.(version) (Reader.IO_reader.from_io io)
    else
      raise (Wrong_protocol_version ((Array.length fs), version))

let write_versioned ?buf version (f : Msg_buffer.t -> 'a -> unit) io (x : 'a) =
  let buf = get_buf buf in
    f buf x;
    IO.write_ui16 io version;
    IO.nwrite io (Msg_buffer.contents buf)
