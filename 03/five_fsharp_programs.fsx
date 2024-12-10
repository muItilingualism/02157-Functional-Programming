(*
Write F#-programs having the types 
(1) bool -> bool, 
(2) int -> int -> int, 
(3) 'a -> 'b -> 'a
(4) 'a -> ('a -> 'b) -> 'b
*)

//inverts a bool
let f1 b = match b with
           | true -> false
           | false -> true;;
let f1_1 b = not b;;

//multiplies the two args
let f2 n m = match n with
             | 0 -> 0
             | n -> n*m;;
let f2_1 n m = n*m

//just returns x
let f3 x y = x

//applies y to x
let f4 x y = y x;;

