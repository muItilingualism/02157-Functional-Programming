(*Declare an F# function multiplicity x xs to find the number of times the value x occurs
in the list xs.*)
let rec mul (x, xs) =
    match (x,xs) with
    | (_, []) -> 0
    | (x, y::ys) when x=y -> 1 + mul(x, ys)
    | (x, _::ys) -> mul(x, ys);;
