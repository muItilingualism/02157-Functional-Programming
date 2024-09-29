(*
The function add: Poly -> Poly -> Poly

Addition of two polynomials represented by lists is performed by element-wise additions.
For example, (1 + 2x) + (3 + 4x + 5x2 + 6x3) = 4 + 6x + 5x2 + 6x3. Notice that (1 + 2x) is
represented by the list [1;2] and (3 + 4x + 5x2 + 6x3) is represented by [3;4;5;6]. Applying
add on these two lists should give

    add [1;2] [3;4;5;6] = [4;6;5;6]

*)
let rec add (xs: Poly) (ys: Poly) : Poly =
    match xs, ys with
    | xs, [] -> xs
    | [], ys -> ys
    | x::xtail, y::ytail -> let xz = add xtail ytail
                            ((x+y)::xz);;
