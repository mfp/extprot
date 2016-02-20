type 'a sum = A | B of 'a | C | D of string

type ('a, 'b, 'c) t2 = ('a * 'b * 'c) list

let id x = x
