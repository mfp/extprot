
[%%extprot.fieldmod Extprot.Field]

(* Either uncomment the following line, equivalent to  -assume-subsets all
 * for extprotc *)
(* [%%extprot.assume_subsets] *)
(* or add the [@@assume_subset] to each message which has got subsets
 * (equivalent to giving the module name to -assume-subsets). *)

type%message simple_bool = { v : bool }
type%message simple_byte = { v : byte }
type%message simple_int = { v : int }
type%message simple_long = { v : Int64.t }
type%message simple_float = { v : float }
type%message simple_string = { v : string }

type%extprot ('a, 'b) tuple = 'a * 'b

type%message simple_tuple = { v : (int, bool) tuple }

type%extprot digest = string
[@@type
  type t = Digest_type.t
  let to_t = Digest_type.from_string
  let from_t = Digest_type.to_string ]

type%message simple_digest = { digest : digest }
type%message simple_digest2 = { digest : digest; extra : string }

type%message msg_sum =
    A of { b : bool }
  | B of { i : int }

type%extprot ('a, 'b, 'c) sum_type = A of 'a | B of 'b | C of 'c | D

type%extprot 'a sum_type2 = Dummy_type.sum = A | B of 'a | C | D of string

type%message simple_sum = { v : (bool, byte, string) sum_type }

type%message test_constructor_order_kept = { v : int sum_type2 }

type%message nested_message = { v : simple_sum; b : int }

type%message lists_arrays = { lint : int list; abool : bool array }

type%extprot complex_rtt =
  A of {
    a1 : ( int * bool list ) list;
    a2 : (int, string, Int64.t) sum_type list;
    }
| B of {
    b1 : bool;
    b2 : string * int list
  }

type%extprot silly_tuple = simple_sum * simple_sum

type%message msg1 = { a : int }
type%extprot msg1a = A of { a : int } | B of { b : string }
type%extprot msg1b = A of { a : int; a' : int } | B of { b : string; b' : string }

type%extprot 'a int_or_stuff = Int of int | Stuff of 'a

type%message msg1c = { a : string int_or_stuff }

type%extprot color = Red | Black
type%extprot node = int * color * silly_tuple

type%message msg1d = { a : node }

type%extprot node2 = Node of int * color * silly_tuple
type%message msg1e = { a : node2 }

type%message msg2 = { a : int }
type%message msg2a = { a : int; b : (bool, byte, int) sum_type }
type%message msg2b = { a : int; b : simple_sum }
type%message msg2c = { a : int; b : int list; c : byte array }
type%message msg2c0 = { a : int; b : int list }
type%message msg2d = { a : int; b : bool }

type%message msg3 = { v0 : int }
type%message msg3a = { v0 : int; v1 : silly_tuple; v2 : int list  }

type%extprot ('a, 'b) record = { a : 'a; b : 'b }
type%extprot 'a irecord = (int, 'a) record

type%extprot rec_message = string irecord
type%extprot rec_message_sum = A of string irecord | B of int irecord
type%message rec_fields = { a : string irecord; b : int }

type%extprot ('a, 'b, 'c, 'd) widen = { a : 'a; b : 'b; c : 'c; d : 'd }

type%message widen1 = (byte, int, Int64.t, byte) widen
type%message widen2 = (int, Int64.t, float, Int64.t) widen
type%message widen3 = (Int64.t, float, float, float) widen

type%message prim_promotion0 = { v : string }
type%message prim_promotion1 = { v : string; foo : (int, int, int) sum_type }
type%message prim_promotion2 = { v : prim_promotion1; }

type%extprot 'a ocaml_type_poly1 = (string * 'a) list
[@@type
  type 'a t = (string * 'a) list
  let to_t = List.rev
  let from_t = List.rev
  let default = [] ]

type%message otp1i = { f : int ocaml_type_poly1 }
type%message otp1f = { f : float ocaml_type_poly1 }

type%extprot ('a, 'b) ocaml_type_poly2 = ('a * 'b) list
[@@type
  type ('a, 'b) t = ('a * 'b) array
  let to_t = Array.of_list
  let from_t = Array.to_list
  let default = [||] ]

type%extprot 'a otp2i = { i : (int ocaml_type_poly1, 'a) ocaml_type_poly2; }
type%message otp2if = { f : float otp2i; }

type%extprot ('a, 'b, 'c) ocaml_type_poly3 = ('a * 'b * 'c) list
[@@type
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Dummy_type.t2
  let to_t = Dummy_type.id
  let from_t = Dummy_type.id
  let default = [] ]

type%extprot 'a otp3i = { i : (int, 'a, int) ocaml_type_poly3; }
type%message otp3if = { f : float otp3i; }

type%extprot sum_ext1 = A of int | B
type%extprot sum_ext2 = A of int * (int, string, Int64.t) sum_type * bool | B

type%message se_1 = { x : sum_ext1; y : string; z : sum_ext1; }
type%message se_2 = { x : sum_ext2; y : string; z : sum_ext2; }

type%message subset__a    = { a : int; b : bool; c : string; d : Int64.t } [@@assume_subset]
type%subset subset__a_1  = subset__a [@@include b, d]
type%subset subset__a_1b = subset__a [@@exclude a, c]
type%subset subset__a_2  = subset__a [@@include a, c]
type%subset subset__a_2b = subset__a [@@exclude b, d]

type%extprot subset__b_0     = { a : int; b : bool; c : string; d : Int64.t } [@@assume_subset]
type%extprot subset__b    = subset__b_0
type%subset subset__b_1  = subset__b [@@include b, d]
type%subset subset__b_1b = subset__b [@@exclude a, c]
type%subset subset__b_2  = subset__b [@@include a, c]
type%subset subset__b_2b = subset__b [@@exclude b, d]

type%extprot ('a, 'b, 'c) subset__c_0 = { a : 'a; b : 'b; c : 'c; d : Int64.t }
type%extprot subset__c    = (int, bool, string) subset__c_0 [@@assume_subset]
type%subset subset__c_1  = subset__c [@@include b, d]
type%subset subset__c_1b = subset__c [@@exclude a, c]
type%subset subset__c_2  = subset__c [@@include a, c]
type%subset subset__c_2b = subset__c [@@exclude b, d]

type%message subset__d    = { a : int; b : bool; c : int list } [@@assume_subset]
type%message subset__d_1  = { a : int }
type%subset subset__d_2  = subset__d [@@include b, c]

type%message subset__e    = { a : int; b : bool; c : int list }
type%message subset__e_1  = { a : int }
type%subset subset__e_2  = subset__d [@@include a, c]

type%extprot ('a, 'b, 'c) subset__f_0 = { a : 'a; b : 'b; c : 'c; d : string }
type%message subset__f    = (int, bool, bool) subset__f_0 [@@assume_subset]
type%message subset__f_1  = { a : int }
type%subset subset__f_2  = subset__f [@@include c]

type%message subset__g    = { a : int; b : bool } [@@assume_subset]
type%subset subset__g1   = subset__g [@@include b]
type%message subset__g2   = { m1 : subset__g; m2 : subset__g }
type%subset subset__g3   = subset__g2 [@@include (m2 : subset__g1)]

type%extprot ('a, 'b, 'c) subset__h0 = { a : 'a; b : 'b; c : 'c }
type%extprot subset__h    = (int, bool, string) subset__h0 [@@assume_subset]
type%subset subset__h1   = subset__h [@@include c]
type%extprot subset__h2   = (subset__h, subset__h, string) subset__h0 [@@assume_subset]
type%subset subset__h3   = subset__h2 [@@include (b : subset__h1)]

type%message subset__i    = { a : string; b : int }
type%subset subset__i1   = subset__i [@@include (b : float)]

type%extprot 'a opt = 'a option = None | Some of 'a
type%extprot 'a opt2 = B | A of 'a
type%message subset__j1   = { a : int opt2; b : string opt2 }
type%subset subset__j2   = subset__j1 [@@include (b : string opt)]

type%extprot bool_true = bool [@default true]
type%extprot int_42    = int [@default 42]
type%extprot int__43   = int [@default -43]
type%extprot byte_44   = byte [@default 44]
type%extprot long_45   = Int64.t [@default 45L]
type%extprot long__46  = Int64.t [@default -46L]
type%extprot float_pi  = float [@default 3.14]
type%extprot float__pi = float [@default -3.14]

type%message defv1a = { c : color; }

type%message defv1b =
  {
    c  : color;
    b  : bool_true;
    i1 : int_42;
    i2 : int__43;
    b2 : byte_44;
    l1 : long_45;
    l2 : long__46;
    f1 : float_pi;
    f2 : float__pi;
  }

type%message defv1c =
  {
    c  : color;
    b  : (bool [@default true]);
    i1 : (int [@default 42]);
    i2 : (int [@default -43]);
    b2 : (byte [@default 44]);
    l1 : (Int64.t [@default 45]);
    l2 : (Int64.t [@default -46]);
    f1 : (float [@default 3.14]);
    f2 : (float [@default -3.14]);
  }

type%extprot pair_int_42__43 = int_42 * int__43

type%message defv2a = { c : color }

type%message defv2b = { c : color; p : pair_int_42__43 }

type%message defv2c = { c : color; p : (int_42, int__43) tuple }

type%message lazy01 = {
  a : bool [@lazy];
  b : byte [@lazy];
  c : int [@lazy];
  d : Int64.t [@lazy];
  e : float [@lazy];
  f : string [@lazy]
}

type%message lazy01b = {
  a : bool;
  b : byte;
  c : int;
  d : Int64.t;
  e : float;
  f : string
}

type%extprot 'a lazyT = { v : 'a [@lazy] }
type%extprot 'a recT = { v : 'a }
type%message lazy02  = int lazyT
type%message lazy02b = int lazyT lazyT
type%message lazy02c = int recT recT
type%message lazy02d = bool lazyT
type%message lazy03  = { v : int [@lazy]; v2 : (int [@default 42]) [@lazy] }
type%message lazy03b = { v : int [@lazy]; }
type%message lazy04  = int list lazyT
type%message lazy05  = int array lazyT
type%message lazy06  = { x : int; v : int sum_type2 [@lazy]  }
type%message lazy06b = { x : int; v : int sum_type2 }
type%message lazy07  = { x : int; v : (int * float) [@lazy] }
type%message lazy07b = { x : int; v : (int * float) }
type%message lazy08  = { x : int; v : (simple_bool * simple_bool) [@lazy] }
type%message lazy08b = { x : int; v : simple_bool [@lazy] }
type%message lazy09  = { v : simple_bool [@lazy] }
type%message lazy10  = { x : int; v : simple_bool [@lazy] }
type%message lazy10b = { x : int; v : simple_bool }
type%message lazy11  = { v : msg_sum [@lazy] }
type%message lazy12  = { v : (simple_bool * simple_bool) [@lazy] }
type%message lazy13  = { v : (msg_sum * msg_sum) [@lazy] }
type%message lazy14  = { v : msg_sum list [@lazy] }
type%message lazy15  = A of { v : bool [@lazy] } | B of { x : int; v : int sum_type2 [@lazy] }
type%message lazy15b = A of { v : bool } | B of { x : int; }

type%message lazy16  = {
  v1 : simple_bool [@lazy];
  v2 : int * string [@lazy];
  v3 : (string, int) tuple [@lazy];
  v4 : (int, int, int) sum_type [@lazy];
  v5 : (int, string, int) sum_type [@lazy];
  v6 : (simple_bool, int, int) sum_type [@lazy];
  v7 : string list [@lazy];
  v8 : string list [@lazy];
  v9 : int array [@lazy];
  v0 : int array [@lazy];
} [@@assume_subset]

type%message lazy16b  = {
  v1 : simple_bool;
  v2 : int * string;
  v3 : (string, int) tuple;
  v4 : (int, int, int) sum_type;
  v5 : (int, string, int) sum_type;
  v6 : (simple_bool, int, int) sum_type;
  v7 : string list;
  v8 : string list;
  v9 : int array;
  v0 : int array;
}

type%subset lazy16c = lazy16 [@@include v1, v2, v5, v9, v0]
type%subset lazy16d = lazy16 [@@exclude v0]

type%extprot string_hohoho = string [@default "hohoho"]

type%message lazy17 = {
  v1 : (int [@default 789]) [@lazy];
  v2 : string_hohoho [@lazy];
  v3 : (int, int, int) sum_type [@lazy];
  v4 : int list [@lazy];
  v5 : int array [@lazy];
  v6 : simple_bool [@lazy];
}

type%message lazy17b = {
  v1 : int;
  v2 : string_hohoho;
  v3 : (int, int, int) sum_type;
  v4 : int list;
  v5 : int array;
}

type%message lazy17c = {
  v1 : int;
  v2 : string_hohoho;
  v3 : (int, int, int) sum_type;
  v4 : int list;
}

type%message lazy17d = {
  v1 : int;
  v2 : string_hohoho;
  v3 : (int, int, int) sum_type;
}

type%message lazy17e = {
  v1 : int;
  v2 : string_hohoho;
}

type%message lazy17f = {
  v1 : int;
}

type%message lazy18  = { v1 : lazy17 [@lazy]; v2 : lazy17 [@lazy] }
type%message lazy18b = { v1 : lazy17f; }

type%message lazy19  = { x : int; v : string_hohoho lazyT [@lazy] }
type%message lazy19b = { x : int; v: string_hohoho lazyT }
type%message lazy19c = { x : int }

type%message lazy20a = { v1 : string lazyT; v2 : simple_bool lazyT }
type%message lazy20b = { v1 : string lazyT [@lazy]; v2 : simple_bool lazyT [@lazy] }
type%message lazy20c = { v1 : lazy20a [@lazy]; v2 : lazy20a [@lazy] } [@@assume_subset]
type%subset lazy20d = lazy20c [@@include (v2 : lazy20b)]

type%extprot lazy21a  = {
  v1 : simple_bool [@lazy];
  v2 : int * string [@lazy];
  v3 : (string, int) tuple [@lazy];
  v4 : (int, int, int) sum_type [@lazy];
  v5 : (int, string, int) sum_type [@lazy];
  v6 : (simple_bool, int, int) sum_type [@lazy];
  v7 : string list [@lazy];
  v8 : string list [@lazy];
  v9 : int array [@lazy];
  v0 : int array [@lazy];
}

type%message lazy21b =
    A of lazy21a
  | B of simple_string lazyT

type%extprot w0 = bool
[@@type
  type t = Wrapped_types.Wrap_bool.t
  let to_t = Wrapped_types.Wrap_bool.from_x
  let from_t = Wrapped_types.Wrap_bool.to_x
  let default = Wrapped_types.Wrap_bool.from_x true ]

type%extprot w1 = int
[@@type
  type t = Wrapped_types.Wrap_int.t
  let to_t = Wrapped_types.Wrap_int.from_x
  let from_t = Wrapped_types.Wrap_int.to_x]

type%extprot w2 = byte
[@@type
  type t = Wrapped_types.Wrap_int.t
  let to_t = Wrapped_types.Wrap_int.from_x
  let from_t = Wrapped_types.Wrap_int.to_x]

type%extprot w3 = Int64.t
[@@type
  type t = Wrapped_types.Wrap_i64.t
  let to_t = Wrapped_types.Wrap_i64.from_x
  let from_t = Wrapped_types.Wrap_i64.to_x]

type%extprot w4 = float
[@@type
  type t = Wrapped_types.Wrap_float.t
  let to_t = Wrapped_types.Wrap_float.from_x
  let from_t = Wrapped_types.Wrap_float.to_x]

type%extprot w5 = string
[@@type
  type t = Wrapped_types.Wrap_string.t
  let to_t = Wrapped_types.Wrap_string.from_x
  let from_t = Wrapped_types.Wrap_string.to_x]

type%extprot w6 = int * int * int
[@@type
  type t = Wrapped_types.Wrap_tuple_iii.t
  let to_t = Wrapped_types.Wrap_tuple_iii.from_x
  let from_t = Wrapped_types.Wrap_tuple_iii.to_x]

type%extprot w7 = int * string * int
[@@type
  type t = Wrapped_types.Wrap_tuple_isi.t
  let to_t = Wrapped_types.Wrap_tuple_isi.from_x
  let from_t = Wrapped_types.Wrap_tuple_isi.to_x]

type%extprot wsum_ = Wrapped_types.Sum.t = A | B of string
type%extprot w8 = wsum_
[@@type
  type t = Wrapped_types.Wrap_sum.t
  let to_t = Wrapped_types.Wrap_sum.from_x
  let from_t = Wrapped_types.Wrap_sum.to_x
  let default = Wrapped_types.Wrap_sum.from_x WSum_.A ]

type%extprot wrec_ = Wrapped_types.Rec.t = { a : int; b : string }
type%extprot w9 = wrec_
[@@type
  type t = Wrapped_types.Wrap_rec.t
  let to_t = Wrapped_types.Wrap_rec.from_x
  let from_t = Wrapped_types.Wrap_rec.to_x
  let default = failwith "missing wrec_" ]

type%extprot w10 = int list
[@@type
  type t = Wrapped_types.Wrap_list.t
  let to_t = Wrapped_types.Wrap_list.from_x
  let from_t = Wrapped_types.Wrap_list.to_x
  let default = Wrapped_types.Wrap_list.from_x [1; 2; 3] ]

type%extprot w11 = int array
[@@type
  type t = Wrapped_types.Wrap_array.t
  let to_t = Wrapped_types.Wrap_array.from_x
  let from_t = Wrapped_types.Wrap_array.to_x
  let default = Wrapped_types.Wrap_array.from_x [|1; 2; 3|] ]

type%extprot w12 = A | B of w9

type%message lazy22  = { v : digest [@lazy] }
type%message lazy22b = { v : digest }
type%message lazy23_0b = { v : w0 }
type%message lazy23_0 = { v : w0 [@lazy] }
type%message lazy23_1 = { v : w1 [@lazy] }
type%message lazy23_2 = { v : w2 [@lazy] }
type%message lazy23_3 = { v : w3 [@lazy] }
type%message lazy23_4 = { v : w4 [@lazy] }
type%message lazy23_5 = { v : w5 [@lazy] }
type%message lazy23_6 = { v : w6 [@lazy] }
type%message lazy23_7 = { v : w7 [@lazy] }
type%message lazy23_8 = { v : w8 [@lazy] }
type%message lazy23_9 = { v : w9 [@lazy] }
type%message lazy23_10 = { v : w10 [@lazy] }
type%message lazy23_11 = { v : w9 list [@lazy] }
type%message lazy23_12 = { v : w10 list [@lazy] }
type%message lazy23_13 = { v : w11 [@lazy] }
type%message lazy23_14 = { v : w9 array [@lazy] }
type%message lazy23_15 = { v : w10 array [@lazy] }
type%message lazy23_16 = { v : w0 * bool [@lazy] }
type%message lazy23_17 = { v : w12 [@lazy] }

type%message lazy24 = {
  v1 : (int [@default 789]);
  v2 : string_hohoho;
  v3 : (int, int, int) sum_type;
  v4 : int list;
  v5 : int array;
  v6 : simple_bool;
}

type%message lazy24a = lazy24 [@@include v1 [@lazy], v2 [@lazy], v3 [@lazy]]
type%message lazy24b = lazy24 [@@include v4 [@lazy], v5 [@lazy], v6 [@lazy]]
type%message lazy24c = lazy24 [@@include v1 [@lazy], v2, v3 [@lazy] ]

type%message lazy25 = lazy17 [@@include v1 [@eager], v3, v4 ]

type%message autolazy1 =
  {
    v1 : int;
    v2 : lazy24;
  } [@@autolazy]

type%message autolazy1b =
  {
    v1 : int;
    v2 : lazy24 [@eager];
    v3 : lazy24;
    v4 : int [@lazy];
  } [@@autolazy]

type%extprot autolazy2 =
    A of { v1 : int; v2 : lazy24; }
  | B of { v1 : float; v2 : lazy24 }
[@@autolazy]

type%extprot autolazy2b =
    A of { v1 : int; v2 : lazy24; v3 : lazy24 [@eager]; v4 : int [@lazy] }
  | B of { v1 : float; v2 : lazy24; v3 : lazy24 [@eager]; v4 : int [@lazy] }
[@@autolazy]
