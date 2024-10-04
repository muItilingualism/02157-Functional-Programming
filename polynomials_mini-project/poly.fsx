module Poly
(* START OF PART 1 *)

type Poly = int list;;

(**
 * The function add: Poly -> Poly -> Poly
 *
 * Addition of two polynomials represented by lists is performed by element-wise additions.
 * For example, (1 + 2x) + (3 + 4x + 5x^2 + 6x^3) = 4 + 6x + 5x^2 + 6x^3. Notice that (1 + 2x) is
 * represented by the list [1;2] and (3 + 4x + 5x^2 + 6x^3) is represented by [3;4;5;6].
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
 * For example, x · (2 + x^3) = 2x + x^4 and therefore mulX [2; 0; 0; 1] = [0; 2; 0; 0; 1].
 *)
let mulX : Poly -> Poly = function
    | [] -> []
    | xs -> 0::xs;;

(**
 * The multiplication function mul: Poly -> Poly -> Poly
 *
 * The following properties are useful when defining multiplication:
 * 0 · Q(x) = 0
 * (a_0 + a_1 · x + ... + a_n · x^n) · Q(x)
 * = a_0 · Q(x) + x · ((a_1 + a_2 · x + ... + a_n · x^(n−1)) · Q(x))
 * For example, (2 + 3x + x^3) · (1 + 2x + 3x^2) = 2 + 7x + 12x^2 + 10x^3 + 2x^4 + 3x^5.
 *)
let rec mul (xs: Poly) (ys: Poly) : Poly =
    match xs, ys with
    | [], ys -> []
    | xs, [] -> []
    | x::xtail, ys -> let term = mulC x ys 
                      let rest = mul xtail (mulX ys)
                      add term rest;;

(**
 * The function eval: int -> Poly -> int
 *
 * If P(x) is a polynomial and a is an integer, then the eval function should compute the
 * integer value P (a).
 * For example, if P (x) = 2 + 3x + x3 then P (2) = 2 + 3 · 2 + 23 = 16, and, therefore,
 * eval 2 [2; 3; 0; 1] = 16.
 *)
let rec private pow (x: int) (n: int) : int =
    match n with
    | 0 -> 1
    | n -> x * (pow x (n-1));;

let rec private evalHelp (n: int) (d: int) (xs: Poly) : int =
    match xs with
    | [] -> 0
    | x::tail when d=0 -> x + (evalHelp n (d+1) tail)
    | x::tail -> (pow n d)*x + (evalHelp n (d+1) tail);;

let eval (n: int) (xs: Poly) : int = evalHelp n 0 xs;;

(* END OF PART 1 *)

(* START OF PART 2 *)

(**
 * The function isLegal: int list -> bool
 * The function isLegal tests whether an integer lists is a legal representation of a polynomial.
 *)
let rec isLegal (xs: int list) : bool =
    match xs with
    | [] -> true
    | x::tail when x=0 && tail=[] -> false
    | x::tail -> isLegal tail;;

(**
 * The function ofList: int list -> Poly
 * Any integer list can be turned into a legal representation of a polynomial by removal of 0’s
 * occurring at the end of the list. The function ofList should do this.
 *)
let rec ofList xs =
    match xs with
    | [] -> []
    | x::tail ->
        let tailWithoutZeros = ofList tail
        match tailWithoutZeros, x with
        | [], 0 -> []
        | _ -> x::tailWithoutZeros

(**
 * The function toString: Poly -> string
 * Choose an appealing textual representation of a polynomial and declare an associated
 * toString function. You may have a look at the output presented on Page 2.
 *)
let rec toString (xs: Poly) : string =
   match xs with
   | [] -> ""
   | [x] -> $"{x}"
   | x::tail -> $"{x} {toString tail}";;

(* END OF PART 2 *)
