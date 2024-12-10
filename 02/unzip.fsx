let rec unzip xys = 
    match xys with
    | [] -> ([],[])
    | (x,y)::tail -> let (xs, ys) = unzip tail // xs and ys sort of function as the return type!
                     (x::xs, y::ys)
