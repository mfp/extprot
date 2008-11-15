
let serialize f x =
  let b = Msg_buffer.create () in
    f b x;
    Msg_buffer.contents b

let deserialize f s = f (Reader.String_reader.from_string s)
