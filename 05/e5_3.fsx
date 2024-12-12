(*
Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
Test the function on different predicates (e.g., p(x) = x > 0).

Solve the exercise using List.fold or List.foldBack
*)

let sum (p, xs) = List.fold (fun acc x -> if p(x) then x+acc else acc) 0 xs;;
