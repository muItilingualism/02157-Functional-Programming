(*
Write F#-programs having the types 
(1) bool -> bool, 
(2) int -> int -> int, 
(3) 'a -> 'b -> 'a and 
(4) 'a -> ('a -> 'b) -> 'b
*)

#inverts a bool
let f1 b = match b with
           | true -> false
           | false -> true;;

let f2 n m = match n with
             | 0 -> 0
             | n -> n*m;;

