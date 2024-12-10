(*
Consider the declarations:

let rec f = function
    | (x, []) -> []
    | (x, y::ys) -> (x+y)::f(x-1, ys);;

let rec g = function
    | [] -> []
    | (x,y)::s -> (x,y)::(y,x)::g s;;

let rec h = function
    | [] -> []
    | x::xs -> x::(h xs)@[x];;

Find the types for f, g and h and explain the value of the expressions:
1. f(x, [y0,y1, . . . ,yn−1]), n ≥ 0
2. g[(x0, y0),(x1, y1), . . . ,(xn−1, yn−1)], n ≥ 0
3. h[x0,x1, . . . ,xn−1], n ≥ 0
*)

// f: (int, int list) -> int list
// f takes a pair of int and int list and returns a list of ints
// it adds x to the head of the list input to be returned and decrements x and the size of the list for each subsequent recursion until the input list is empty

// g: (a', a') list -> (a', a') list
// g takes a list of a generic pair of a' and returns the same type.
// it reverses every other pair.

// h: a' list -> a' list
// h takes a generic list and returns the same type.
// it mirrors the input list by appending the head in each recursion to the end
