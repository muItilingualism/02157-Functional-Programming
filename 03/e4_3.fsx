(*Declare function evenN: int -> int list such that evenN n generates the list of the first
n non-negative even numbers.*)

let rec evenN = function
    | 0 -> []
    | n when (n % 2 = 0) -> n::evenN(n-1)
    | n -> evenN(n-1);;
