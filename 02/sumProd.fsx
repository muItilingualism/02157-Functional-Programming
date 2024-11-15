# sum: int list -> int
# prod: int list -> int
let rec sumProd1 xs = (sum xs, prod xs);;
# goes through both lists recursively!


let help x (s, p) = (x+s, x*p);;

let rec sumProd2 xs = match xs with
    | [] -> (0, 1)
    | x::tail -> help x sumProd tail;;
# more efficient
