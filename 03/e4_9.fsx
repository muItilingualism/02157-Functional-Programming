(*

Declare an F# function zip such that:

zip([x0;x1; ... ;xn−1],[y0;y1; ... ;yn−1])
= [(x0, y0);(x1, y1); ... ;(xn−1, yn−1)]

The function should raise an exception if the two lists are not of equal length.
*)

let rec len xs =
    match xs with
    | [] -> 0
    | x::tail -> 1+len(tail);;

let rec zip (xs, ys) = 
    match (xs, ys) with
    | ([], []) -> []
    | (x::xtail, y::ytail) when (len(xs)=len(ys)) -> let (zs) = zip (xtail, ytail)
                                                     ((x,y)::zs)
    | _ -> failwith "the lists must be of equal length";;
                     
