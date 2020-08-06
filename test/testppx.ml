type%extprot foo = { a : int; b : float }
type%extprot bar = int
type%extprot baz = { a : bar; b : int * int }
type%extprot t1 = Int64.t
type%extprot t2 = int
type%extprot t3 = int list
type%extprot t4 = float array
type%extprot t5 = int * int * float array
type%extprot ('a, 'b) t6 = int * 'a * 'b array
type%extprot t7 = { a : int; b : (bool, Int64.t) t6 }
type%extprot ('a, 'b) t8 = { a : int; b : 'a * 'b }
type%extprot t9 = { a : int; b : (int, bool array) t8 }
type%extprot t10 = (int, t4) t8
type%extprot t11 = A | B of string
type%extprot t12 = { a : t11; b : Int64.t }
type%extprot t13 = A of { a : int; b : float } | B of { v : bool }
type%extprot t14 = A of t12 | B of { v : bool }
type%message t15 = { v0 : int }
type%extprot t16 = A of t12 | B of { v : bool } | C of t15
type%message t17 = { v0 : int; v1 : t14 }
type%message t18 = { v0 : int; v2 : Foo.Bar.baz }
type%message t19 = A.B.C.foo

type%message subset1 = { a : int; b : float; c : int list }
type%subset subset1a = subset1 [@@include a, c]
type%subset subset1b = subset1 [@@exclude a]

type%message subset2 = { v0 : t17; v1 : subset1; v2 : float list }
type%subset subset2a = subset2 [@@include (v1 : subset1a), v2]
(* type%subset foo = bar [@@exclude a, b, c, d] *)

type%message lazy1 = { v0 : int; v1 : foo [@lazy] }
type%message lazy2 = { v0 : int list; v1 : int * int; v2 : string } [@@autolazy]
type%message lazy3 = { v0 : int list; v1 : int list [@eager]; v2 : int * int; v3 : string } [@@autolazy]

type%subset lazy3a = lazy3 [@@include v1, v2, v3]
type%subset lazy3b = lazy3 [@@include v1[@lazy], v2, v3]
type%subset lazy1a = lazy1 [@@include v1[@eager]]

type%subset subset2b = subset2 [@@include (v1 [@lazy] : subset1a), v2]
type%subset subset2c = subset2 [@@include (v1 [@lazy] : subset1a)]

type%extprot foo1 = A.B.foo = { a : int; b : float }
type%extprot foo2 = A.B.foo2 = A of int | B
type%message bar2 = A.B.bar = { a : int; b : float; c : int }

type%extprot foo3 = string
[@@type type t = A.foo let to_t = A.from_string let from_t = A.to_string]
type%message foobar = { v : foo3 [@lazy] }

type%extprot foo4 = string
[@@type type t = A.foo let to_t = A.from_string let from_t = A.to_string let default = `ABCD ]
type%message foobar2 = { v : foo4 [@lazy] }

type%extprot foo5 = string [@default "foobar"]
type%extprot foo6 = int [@default 42]
type%extprot foo7 = float [@default 42.0]
type%message t20  = { v0 : foo5[@lazy]; v1 : foo6; v2 : foo7 }
type%message t21  = { v0 : string; v1 : (string [@default "foo"]) [@lazy] }
