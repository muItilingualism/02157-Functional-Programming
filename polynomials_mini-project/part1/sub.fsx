(*

The subtraction function sub: Poly -> Poly -> Poly

Subtraction of two polynomials represented by lists is performed by element-wise subtrac-
tions. For example, (1 + 2x) − (3 + 4x + 5x2 + 6x3) = −2 − 2x − 5x2 − 6x3.

*)

let rec inv : Poly -> Poly = function
    | [] -> []
    | x::xs -> (-x) :: inv xs;;

let rec sub (xs: Poly) (ys: Poly) : Poly =
    match xs, ys with
    | [], ys -> inv ys
    | xs, [] -> xs
    | x::xtail, y::ytail -> (x-y) :: sub xtail ytail;;
