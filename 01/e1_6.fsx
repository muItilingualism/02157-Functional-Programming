(*
Declare a recursive function sum: int * int -> int, where
sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
Give the recursion formula corresponding to the declaration.
*)

let rec sum = function
    | (m, 0) -> m
    | (m, n) -> (m+n) + sum(m, n-1);;
// sum: int * int -> int
