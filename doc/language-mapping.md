
## OCaml

extprotc will generate a module named after the input file (by default), with
one module per type or message definition, whose name is
String.capitalize(that of the type/message.

The modules generated for a type declaration will have a signature like

    module Typename : sig
      type typename = ...
      val pp_typename : Format.formatter -> typename -> unit
    end

if the type is polymorphic, it will be like:

    module Tuple :
      sig
        type ('a, 'b) tuple = 'a * 'b
        val pp_tuple :
          (Format.formatter -> 'a -> unit) ->
          (Format.formatter -> 'b -> unit) -> Format.formatter -> 'a * 'b -> unit
      end

The modules generated for messages will include a type for each of the message
cases plus a sum type joining them, in addition to functions to pretty-print
messages, read from a `String\_reader.t` or an `IO\_reader.t`, and write to a
`Msg_buffer.t`.

Example:

    message msg1 = A { a : int } | B { b : string }

will translate to this module:

    module Msg1 :
    sig
      type msg1_A = { a : int; }
      type msg1_B = { b : string; }
      type msg1 = A of msg1_A | B of msg1_B
      val pp_msg1 : Format.formatter -> msg1 -> unit
      val read_msg1 : Extprot.Reader.String_reader.t -> msg1
      val fast_io_read_msg1 : Extprot.Reader.IO_reader.t -> msg1
      val io_read_msg1 : Extprot.Reader.IO_reader.t -> msg1
      val write_msg1 : Extprot.Msg_buffer.t -> msg1 -> unit
    end

### Performance

See the `test/bm_01` program.
It (de)serializes a large number of random `complex_rtt` messages, defined as
follows:

    type sum_type 'a 'b 'c = A 'a | B 'b | C 'c | D

    message complex_rtt =
      A {
	a1 : [ ( int * [bool] ) ];
	a2 : [ sum_type<int, string, long> ]
	}
    | B {
	b1 : bool;
	b2 : (string * [int])
      }

The lengths of the lists and strings vary randomly from 0 to 9.

Here are the results for 200000 messages with the implementation as of
2008-11-07. Note that the extprot reader has only undergone some basic
optimization, and the writer is completely unoptimized (it currently copies
data repeatedly).

    $ ./bm_01 -n 200000
    [gen array of length 200000]  1.20453s

    ==== extprot IO_reader ====
    ** Serialized in 16085760 bytes.
    [write]  1.02979s
    [read] min:  0.38069s   avg:  0.38117s

    ==== Marshal (individual msgs) ====
    ** Serialized in 17662020 bytes.
    [write]  0.58272s
    [read] min:  0.23324s   avg:  0.27496s

    ==== Marshal (array) ====
    ** Serialized in 13662045 bytes.
    [write]  0.40134s
    [read] min:  0.28890s   avg:  0.30420s

    ==== extprot String_reader ====
    ** Serialized in 16085760 bytes.
    [write]  0.93219s
    [read] min:  0.29374s   avg:  0.29550s

`Marshal (array)` uses `Marshal.to_string array [Marshal.No_sharing]`;
`Marshal (individual msgs)` uses `Marshal.to_string` for each individual
message and appends the serialized form to a `Buffer.t`.
