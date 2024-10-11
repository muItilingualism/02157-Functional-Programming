(*
Problem 2 (20%)
1. Declare a function mixMap so that
    mixMap f [x_0; x_1; ... ; x_m] [y_0; y_1; ... ; y_m] = [f (x_0, y_0); f (x_1, y_1); ... ; f (x_m, y_m)]
*)
let rec mixMap f xs ys : 'a list =
    match xs, ys with
    | [], [] -> []
    | xs, [] -> xs
    | [], ys -> ys
    | x::xtail, y::ytail -> let (zs) = mixMap f xtail ytail
                            (f(x,y)::zs)

(*
2. Declare a function unmixMap so that
    unmixMap f g [(x_0, y_0); (x_1, y_1); ... ; (x_n, y_n)] = ([f x_0; f x_1; ... ; f x_n], [g y_0; g y_1; ... ; g y_n])
*)

(*
3. Give the most general types for mixMap and unmixMap
*)

//mixMap 'a * 'a -> 'a -> 'a list -> 'a list -> 'a list
