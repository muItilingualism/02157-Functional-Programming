(**
 * Problem 2 (Approx 25 minutes)
 *
 * Consider the declaration:
 *
 *)
let rec splitAt i xs =
    if i<=0 then ([],xs)
    else match xs with
         | [] -> ([],[])
         | x::tail -> let (xs1,xs2) = splitAt (i-1) tail
                      (x::xs1,xs2);;

(**
 * 1. What are the values of
 * - splitAt -1 [1;2;3],
 * - splitAt 3 [1;2;3;4;5] and
 * - splitAt 4 [1;2;3].
 *)
# The value of splitAt -1 [1;2;3] is ([], [1;2;3]) 
#   as the index is less than 0, hence the function exits immediately in the if branch 
#   and we do no recursion.

# The value of splitAt 3 [1;2;3;4;5] is ([1;2;3], [4;5]).
# -> splitAt 3 [1;2;3;4;5]
# -> splitAt 2 [2;3;4;5] ([1]), [])
# -> splitAt 1 [3;4;5] ([1;2], [])
# -> splitAt 0 [4;5] ([1;2;3], [])
# -> ([1;2;3], [4;5]) 

# The value of splitAt 4 is ((1;2;3], []), when the index is greater than the length of this list we do not recurse and return ([], []) - the first match case - stopping the recursion.
# -> splitAt 4 [1;2;3]
# -> splitAt 3 [2;3] ([1], [])
# -> splitAt 2 [3] ([1;2], [])
# -> splitAt 1 [] ([1;2;3], [])
# -> splitAt 0 [] ([1;2;3], [])
# -> (1;2;3], [])

(**
 * 2. What is the type of splitAt? Justify your answer briefly.
 *)
# splitAt: i: int -> xs: 'a list -> 'a list * 'a list
# `i` is an int as we are using the minus operator when recursing. Also the comparison with 0...
# `xs` is a generic list as we do not alter the contents of the lists, nor need equality etc.
# The return type is a tuple as revealed by the base cases and has to be the same type as `xs`.

(**
 * 3. Describe what splitAt is computing by stating the value of
 * 
 * splitAt k [x_0; x_1; ... ; x_(n−1)], where n ≥ 0.
 *)
# `splitAt` splits the given list at k meaning on the left side of x_k. 
# The value is (x_0; x_1; ... ; x_(k-1)], [x_k; x_(k+1); ... ; x_(n-1)]
#                                                   ^
#                                                not plus!

# correct answer:
# We divide into 3 cases alike the 3 examples above
# k<=0 splitAt k xs = ([], xs)
# 1<=k<n splitAt k xs = ([x_0; x_1; ...; x_(k-1)],[x_l;...;x_(n-1)])
#k>=n ...
