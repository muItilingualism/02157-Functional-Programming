(*
A teacher named Robin has a bookshelf with book that are lent to colleagues and students.
The following models of the shelf and the loans are introduced to keep track of books:
*)

type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

(*
Books are just modelled by strings and we assume below that the books appear in
alphabetic order in a shelf. (Built-in orderings <, <=, etc. can be used to compare books.)
A shelf may contain multiple copies of the same book.
A loan is modelled by a triple (b, n, d), where b is a book, n the name of the borrower
and d the date when the book was borrowed. Names are strings and dates are integers.

Consider, for example, the following declarations of a shelf sh0 with three books and a list
ls0 containing four loans.
*)

let sh0 = ["Introduction to meta-mathematics";
           "To mock a mockingbird";
           "What is the name of this book"];;

let ls0 = [("Communication and concurrency", "Bob", 4);
           ("Programming in Haskell", "Paul", 2);
           ("Communicating Sequential processes", "Mary", 7);
           ("Elements of the theory of computation", "Dick", 1)];;

(*
The questions 1. to 6. in this problem should be solved without using functions
from the libraries List, Seq, Set and Map. That is, the requested functions should
be declared using explicit recursion.

In the declarations you can assume that books are ordered alphabetically in shelf arguments
to functions. It is required that books are ordered alphabetically in shelves returned by
functions.

1. Declare a function onShelf: Book -> Shelf -> bool that can check whether a book
   is on a shelf.
*)

let rec onShelf (b: Book) (ss: Shelf): bool =
    match ss with
    | [] -> false;
    | sb::tail when b=sb -> true
    | _::tail -> onShelf b tail;;

(*
2. Declare a function toShelf: Book -> Shelf -> Shelf so that toShelf b bs is the
   shelf obtained from bs by insertion of b in the right position.
*)

//assuming A < B is alphabetically ordered

let rec toShelf (b: Book) (ss: Shelf): Shelf =
    match ss with
    | [] -> [b]
    | sb::tail when b <= sb -> b::sb::tail
    | sb::tail -> sb::(toShelf b tail);;

(*
3. Declare a function fromShelf: Book -> Shelf -> Shelf option. The value of the
   expression fromShelf b bs is None if bs does not contain b. Otherwise, the value is
   Some bs′, where bs′ is obtained from bs by deletion of one occurrence of b.
*)
let rec deleteOne (b: Book) (ss: Shelf): Shelf =
    match ss with
    | [] -> [];
    | sb::tail when b=sb -> tail
    | sb::tail -> sb::(deleteOne b tail);;

let rec fromShelfHelp (b: book) (ss: Shelf) (acc: Shelf option): Shelf option =
    match ss with
    | [] -> acc
    | sb::tail when b=sb -> Some (acc @ tail)
    | sb::tail -> sb::(fromShelfHelp b ss acc tail)

let rec fromShelf (b: Book) (ss: Shelf): Shelf option =
    match ss with
    | [] -> None
    | sb::tail when b=sb -> delete
    | _::tail -> fromShelf b tail;;

