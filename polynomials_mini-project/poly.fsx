module Poly
(* START OF PART 1 *)

type Poly = int list;;

(**
 * The function add: Poly -> Poly -> Poly
 *
 * Addition of two polynomials represented by lists is performed by element-wise additions.
 * For example, (1 + 2x) + (3 + 4x + 5x2 + 6x3) = 4 + 6x + 5x2 + 6x3. Notice that (1 + 2x) is
 * represented by the list [1;2] and (3 + 4x + 5x2 + 6x3) is represented by [3;4;5;6].
 * Applying add on these two lists should give
 *
 *     add [1;2] [3;4;5;6] = [4;6;5;6]
 *)
let rec add (xs: Poly) (ys: Poly) : Poly =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::xtail, y::ytail -> (x+y) :: add xtail ytail

(**
 * The function mulC: int -> Poly -> Poly
 *
 * The function mulC should implement the multiplication of a polynomial by a constant.
 * For example, 2 · (2 + x^3) = 4 + 2x^3 and therefore mulC 2 [2; 0; 0; 1] = [4; 0; 0; 2].
 *)
let rec mulC (n: int) (xs: Poly) : Poly =
    match xs with
    | [] -> []
    | x::tail -> (n*x) :: mulC n tail;;

(**
 * The subtraction function sub: Poly -> Poly -> Poly
 *
 * Subtraction of two polynomials represented by lists is performed by element-wise subtractions. 
 * For example, (1 + 2x) − (3 + 4x + 5x2 + 6x3) = −2 − 2x − 5x2 − 6x3.
 *)
let rec private inv : Poly -> Poly = function
    | [] -> []
    | x::xs -> (-x) :: inv xs;;

let rec sub (xs: Poly) (ys: Poly) : Poly =
    match xs, ys with
    | [], ys -> inv ys
    | xs, [] -> xs
    | x::xtail, y::ytail -> (x-y) :: sub xtail ytail;;

(**
 * The function mulX: Poly -> Poly
 *
 * The multiplication function mulX should implement the multiplication of a polynomial by x.
 * For example, x · (2 + x3) = 2x + x4 and therefore mulX [2; 0; 0; 1] = [0; 2; 0; 0; 1].
 *)
let mulX : Poly -> Poly = function
    | [] -> []
    | xs -> 0::xs;;

(* END OF PART 1 *)
