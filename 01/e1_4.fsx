(*
Declare a recursive function f: int -> int, where
f(n) = 1 + 2 + · · · + (n − 1) + n
for n ≥ 0. (Hint: use two clauses with 0 and n as patterns.)
State the recursion formula corresponding to the declaration.
Give an evaluation for f(4).
*)

let rec f = function
    | 0 -> 0
    | n -> n + f(n-1);;
