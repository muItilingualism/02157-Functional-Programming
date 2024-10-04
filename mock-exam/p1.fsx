(**
 * Problem 1 (Approx 30 minutes)
 *
 * All questions in this problem should be solved without using functions from
 * the libraries List, Seq, Set and Map.
 *)

(**
 * 1. Declare a function numberOf x ys that returns the number of times x occurs in the list
 * ys. For example, numberOf 2 [0;2;3;3;0;2;4;2;1] = 3.
 *)
let rec numberOf x ys : int =
    match ys with
    | [] -> 0
    | y::tail when x=y -> 1 + numberOf x tail
    | _::tail -> numberOf x tail;;

(**
 * 2. Declare a function positionsOf x ys that returns the list containing the positions of
 * occurrences of x in the list ys. For example, positionsOf 2 [0;2;3;3;0;2;4;2;1]
 * = [1; 5; 7]. Notice that the position of the first element of a non-empty list is 0.
 *
 * Hint: You may consider introducing a helper function.
 *)
let rec positionsOfHelper i x ys =
    match ys with
    | [] -> []
    | y::tail when x=y -> i::positionsOfHelper (i+1) x tail
    | _::tail -> positionsOfHelper (i+1) x tail;;

let positionsOf x ys = positionsOfHelper 0 x ys;;

(**
 * 3. Declare a function filterMap: (’a->bool) -> (’a->’b) -> ’a list -> ’b list.
 * The value of filterMap p f xs is the list obtained from xs by applying f to the
 * elements that satisfy the predicate p.
 *
 * For example, filterMap (fun x -> x>=2) string [0;2;3;3;0;2;4;2;1] returns the
 * list [”2”; ”3”; ”3”; ”2”; ”4”; ”2”].
 *)
let rec filterMap (p: 'a->bool) (f: 'a->'b) (xs: 'a list) : 'b list =
    match xs with
    | [] -> []
    | x::tail when (p x) -> (f x)::(filterMap p f tail)
    | _::tail -> filterMap p f tail;;
