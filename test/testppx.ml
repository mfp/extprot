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
(* type%extprot t10 = (int, t4) t8 *)
