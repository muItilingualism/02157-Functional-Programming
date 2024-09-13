(*The following figure gives the first part of Pascal's triangle:
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
The entries of the triangle are called binomial coefficients. The k'th binomial coefficient of the
n'th row is denoted (n over k), for n ≥ 0 and 0 ≤ k ≤ n. For example, (2 over 1) = 2 and (4 over 2) = 6. The first
and last binomial coefficients, that is, (n over 0) and (n over n), of row n are both 1. A binomial coefficient
inside a row is the sum of the two binomial coefficients immediately above it. These properties
can be expressed as follows: (n over 0) = (n over n) = 1 and (n over k) = (n − 1 over k − 1) + (n − 1 over k)
if n ≠ 0, k ≠ 0, and n > k.
Declare an F# function bin: int * int -> int to compute binomial coefficients.*)

let rec bin = function
    | (_, 0) -> 1
    | (n, k) when (n <> 0) && (k <> 0) && (n > k) -> bin(n-1, k-1) + bin(n-1, k)
    | (_, _) -> 1
