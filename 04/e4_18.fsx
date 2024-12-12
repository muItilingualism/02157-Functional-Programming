(*
Consider the declaration:

let rec f g = function
    | [] -> []
    | x::xs -> g x :: f (fun y -> g(g y)) xs;;

Find the type for f and explain the value of the expression:

    f g [x0; x1; x2; ... ; xn−1]
*)

// f is a function which takes two arguments, a list and a function g, and returns a list.
// f: ('a-> 'a) -> 'a list -> 'a list
// g is a function which maps from 'a to 'a.

// f splits the incoming list and applies the supplied function g to the head, before merging this result with the tail and repeating recursively
// for each successive call g is applied twice. Meaning, x0 it is applied once, x1 it is applied twice, x2 it is applied four times, x3 it is applied eight times, etc (2^n).
// hence g is transformed to on each subsequent recursive call (g is composed with itself).
