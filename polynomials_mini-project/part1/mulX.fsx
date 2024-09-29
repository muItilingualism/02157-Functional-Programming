(*

The function mulX: Poly -> Poly

The multiplication function mulX should implement the multiplication of a polynomial by
x. For example, x · (2 + x3) = 2x + x4 and therefore mulX [2; 0; 0; 1] = [0; 2; 0; 0; 1].

*)

let mulX : Poly -> Poly = function
    | [] -> []
    | xs -> 0::xs;;
