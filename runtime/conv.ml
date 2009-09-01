
exception Wrong_protocol_version of int * int (* max_known, found *)

let get_buf = function
    None -> Msg_buffer.create ()
  | Some b -> Msg_buffer.clear b; b

let serialize ?buf f x =
  let b = get_buf buf in
    f b x;
    Msg_buffer.contents b

let deserialize f s = f (Reader.String_reader.from_string s)

let serialize_versioned ?buf fs version x =
  let buf = get_buf buf in
    if version < 0 || version > 0xFFFF || version >= Array.length fs then
      invalid_arg ("Extprot.Conv.serialize_versioned: bad version " ^
                   string_of_int version);
    Msg_buffer.add_byte buf (version land 0xFF);
    Msg_buffer.add_byte buf ((version lsr 8) land 0xFF);
    fs.(version) buf x;
    Msg_buffer.contents buf

let deserialize_versioned fs s =
  let len = String.length s in
    if len < 2 then
      raise (Wrong_protocol_version (Array.length fs, -1));
    let version = Char.code (s.[0]) + (Char.code s.[1] lsl 8) in
      if version < Array.length fs then
        fs.(version) (Reader.String_reader.make s 2 (len - 2))
      else
        raise (Wrong_protocol_version (Array.length fs, version))

let read f io = f (Reader.IO_reader.from_io io)

let write ?buf (f : Msg_buffer.t -> 'a -> unit) io (x : 'a) =
  let buf = get_buf buf in
    f buf x;
    Msg_buffer.output_buffer_to_io io buf

let read_versioned fs rd =
  let a = Reader.IO_reader.read_byte rd in
  let b = Reader.IO_reader.read_byte rd in
  let version = a + b lsl 8 in
    if version < Array.length fs then
      fs.(version) rd
    else begin
      let hd = Reader.IO_reader.read_prefix rd in
        Reader.IO_reader.skip_value rd hd;
        raise (Wrong_protocol_version ((Array.length fs), version))
    end

let io_read_versioned fs io = read_versioned fs (Reader.IO_reader.from_io io)

let write_versioned ?buf fs version io x =
  let buf = get_buf buf in
    if version < 0 || version > 0xFFFF || version >= Array.length fs then
      invalid_arg ("Extprot.Conv.write_versioned: bad version " ^
                   string_of_int version);
    fs.(version) buf x;
    IO.write_ui16 io version;
    Msg_buffer.output_buffer_to_io io buf
