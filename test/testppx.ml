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
