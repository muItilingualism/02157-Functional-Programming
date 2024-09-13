(*Declare an F# function pow: string * int -> string, where:
pow(s, n) = s · s · ... · s
           ︸ ︷︷︷︷︷ ︸
                  n
where we use · to denote string concatenation. (The F# representation is +.)*)

let rec pow = function
    | (_, 0) -> ""
    | (s, n) -> s + pow(s, n-1);;
