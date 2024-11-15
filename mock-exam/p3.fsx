(**
 * Problem 3 (Approx 20 minutes)
 *
 * Consider the following F# declarations:
 *
 * let rec f(xs,rs) = match xs with
 *                    | [] -> rs
 *                    | [x] -> x::rs
 *                    | x1::x2::xs -> x1::f(xs,x2::rs)
 * let g xs = f(xs,[]);;
 let rec f(xs, rs) = match xs with
                     | [] -> rs
                     | [x] -> x::rs
                     | x1::x2::xs -> x1::f(xs, x2::rs);;

(**
 * 1. Give a step-by-step evaluation (using ->) for g [1;2;3;4;5] determining the value of
 * the expression. There should at least be one step for every recursive call of f.
 *)
# g [1;2;3;4;5]
# -> f ([1;2;3;4;5], [])
# -> 1::f([3;4;5], 2::[])
# -> [1]::3::f([5], 4::[2])
# -> [1;3]::5::[4;2]
# -> [1;3;5;4;2]


(**
 * 2. Give the types for f and g, and describe what g computes. Your description should
 * focus on what it computes, rather than on individual computation steps.
 *)
# f: 'a list * 'a list -> 'a list
# g: 'a list -> 'a list

# g shuffles the list xs so that every other value is sent to the end of the list
# in a sorted list ...
# g [x_0;x_1; ...;x_(n-1) produces [x_0;x_2;x_4;...;x_(n-1);???]
