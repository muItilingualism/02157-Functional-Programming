(*
Declare a function f: int -> bool such that f(n) = true exactly when n is divisible by 2
or divisible by 3 but not divisible by 5. Write down the expected values of f(24), f(27), f(29)
and f(30) and compare with the result. Hint: n is divisible by q when n%q = 0.
*)

let f = function
    | n when ((n%3=0 || n%2=0) && n%5<>0) -> true
    | _ -> false;;
//f(24) = true, f(27) = true, f(29) = false, f(30) = false
