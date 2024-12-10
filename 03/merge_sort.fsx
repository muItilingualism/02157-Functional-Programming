(*
You shall develop a simple version of merge sort, an interesting sorting algorithm that
has an n log n worst-case execution time. The purpose of this particular exercise is to get
practice in the development of elegant functional programs on lists – not to develop efficient
sorting programs. Furthermore, you should achieve a basic understanding of computations
of recursive functions on lists.

Strive for succinctness and elegance when you solve this problem — it is important that
your programs and program designs can be communicated to other people.

Remarks:

• We shall later in the course study techniques addressing efficiency.
• There are efficient sorting functions in the .NET libraries, for example, List.sort.

The merge sort algorithm can be expressed by a functional composition using two functions:
merge and split, where merge combines two sorted lists into a single sorted list, and split
extracts to lists of almost the same sizes from a given list.

The merge function

A merge of two sorted lists, e.g. merge [1;4;9;12] [2;3;4;5;10;13]) is a new sorted list,
[1;2;3;4;4;5;9;10;12;13], made up from the elements of the arguments.

Declare and test this function so that you are sure that all branches of the declaration
work correctly.
*)

let rec merge xs ys =
    match ys with
    | [] -> xs
    | y::ytail -> match xs with
                  | [] -> ys
                  | x::xtail when y>x -> x::merge xtail (y::ytail)
                  | x::xtail when y<x -> y::merge (x::xtail) ytail
                  | x::xtail -> x::y::merge xtail ytail;;

let rec merge2 xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::xtail, y::ytail ->
        if y > x then x::merge2 xtail (y::ytail)
        elif y < x then y::merge2 (x::xtail) ytail
        else x::y::merge2 xtail ytail


(*
The split function

Declare a function to split a list into two lists of (almost) the same lengths. You may
declare the function split such that
split [x0; x1; x2; x3; . . . ; xn−1] = ([x0; x2; . . .], [x1; x3; . . .])

Declare and test this function so that you are sure that all branches of the declaration
work correctly.
*)

let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x::y::tail -> let (xs, ys) = split tail
                    (x::xs, y::ys);;

(*
The sort function

The idea behind top-down merge sort is a recursive algorithm: take an arbitrary list xs
with more than one element and split it into two (almost) equal-length lists: xs1 and xs2.
Sort xs1 and xs2 and merge the results. The empty list and lists with just one element are
the base cases.

Declare a function for top-down merge sort in F#.
Test this function so that you are sure that all branches of the declaration work correctly.
*)

let rec sort xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | xs -> let (ys, zs) = split xs
            (merge (sort ys) (sort zs));;
