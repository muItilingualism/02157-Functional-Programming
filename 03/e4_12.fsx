(*
Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
Test the function on different predicates (e.g., p(x) = x > 0).
*)

let p1 x = x > 0;;
let p2 x = x = 10;;

let rec sum(p, xs) =
    match xs with
    | [] -> 0
    | x::tail when (p(x)) -> x+sum(p, tail)
    | _::tail -> sum(p, tail);;
