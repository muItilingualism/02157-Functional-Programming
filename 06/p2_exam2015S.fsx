(*
Problem 2 (20%)
1. Declare a function mixMap so that
    mixMap f [x_0; x_1; ... ; x_m] [y_0; y_1; ... ; y_m] = [f (x_0, y_0); f (x_1, y_1); ... ; f (x_m, y_m)]
*)
exception VaryingLengths
//assuming equal lengths
let rec mixMap f xs ys =
    match xs, ys with
    | [], [] -> []
    | [], _ -> raise VaryingLengths
    | _, [] -> raise VaryingLengths
    | x::xtail, y::ytail -> let (zs) = mixMap f xtail ytail
                            (f(x,y)::zs);;

let mixMap1 f xs ys =
    List.map2 (fun x y -> f (x, y)) xs ys;;

(*
2. Declare a function unmixMap so that
    unmixMap f g [(x_0, y_0); (x_1, y_1); ... ; (x_n, y_n)] = ([f x_0; f x_1; ... ; f x_n], [g y_0; g y_1; ... ; g y_n])
*)
let rec unmixMap f g xs : ('a list * 'b list) =
    match xs with
    | [] -> ([],[])
    | [(x,y)] -> ([f x], [g y])
    | (x,y)::tail -> let (xs, ys) = unmixMap f g tail
                     ((f x)::xs, (g y)::ys);;

let unmixMap1 f g xs =
    let (xs, ys) = List.unzip xs
    (List.map f xs, List.map g ys);;

(*
3. Give the most general types for mixMap and unmixMap
*)

//mixMap ('a * 'b -> 'c) -> 'a list -> 'b list -> 'c list
//unmixMap ('a -> 'b) -> ('c -> 'd) -> ('a * 'c) list -> 'b list * 'd list
