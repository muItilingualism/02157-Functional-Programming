(*

The function mulC: int -> Poly -> Poly

The function mulC should implement the multiplication of a polynomial by a constant. For
example, 2 · (2 + x^3) = 4 + 2x^3 and therefore mulC 2 [2; 0; 0; 1] = [4; 0; 0; 2].

*)

let rec mulC (n: int) (xs: Poly) : Poly =
    match xs with
    | [] -> []
    | x::tail -> (n*x) :: mulC n tail;;
