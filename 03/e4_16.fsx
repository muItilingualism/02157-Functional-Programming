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

# f: (int, int list) -> int list
# f takes a pair of int and list and returns a list of ints

# g: (a', a') list -> (a', a') list
# g takes a list of a generic pair of a' and returns the same type.

# h: a' list -> a' list
# h takes a generic list and returns the same type.
