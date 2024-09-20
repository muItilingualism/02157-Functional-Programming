(*
Declare an F# function split such that:
    split [x0;x1;x2;x3; ... ;xn−1] = ([x0;x2; ... ], [x1;x3; ... ])
*)

let rec split xs =
    match xs with 
    | [] -> ([],[])
    | x::rest when (x % 2 = 0) -> let (ys, zs) = split rest
                                  (x::ys, zs)
    | x::rest -> let (ys, zs) = split rest
                 (ys, x::zs);;
