
let serialize f x =
  let b = Msg_buffer.create () in
    f b x;
    Msg_buffer.contents b

let deserialize f s = f (Reader.String_reader.from_string s)

let read f io = f (Reader.IO_reader.from_io io)

let write ?buf (f : Msg_buffer.t -> 'a -> unit) io (x : 'a) =
  let buf = match buf with
      None -> Msg_buffer.create ()
    | Some b -> Msg_buffer.clear b; b
  in
    f buf x;
    IO.nwrite io (Msg_buffer.contents buf)
