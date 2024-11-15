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

let rec fromShelfHelp (b: Book) (ss: Shelf): Shelf =
    match ss with
    | [] -> [] //can never be reached?
    | sb::tail when b=sb -> tail
    | sb::tail -> sb::(fromShelfHelp b tail)
    

let fromShelf (b: Book) (ss: Shelf): Shelf option =
    if (not (onShelf b ss)) then
        None
    else
        Some (fromShelfHelp b ss);;

(*
4. Declare a function addLoan b n d ls, that adds the loan (b, n, d) to the list of loans ls.
   Furthermore, declare a function removeLoan b n ls. The value of the function is the list
   obtained from the list of loans ls by deletion of the first element of the form (b, n, d),
   where d is some date, if such an element exists. Otherwise ls is returned. For example,
   removeLoan "Programming in Haskell" "Paul" ls0 gives the list
      
      [("Communication and concurrency", "Bob", 4);
       ("Communicating Sequential processes", "Mary", 7);
       ("Elements of the theory of computation", "Dick", 1)]|
*)
// assuming arbitrary ordering

let addLoan (b: Book) (n: Name) (d: Date) (ls: Loan list): Loan list =
    match ls with
    | [] -> [Loan(b, n, d)]
    | _ -> Loan(b, n, d)::ls;;

let rec removeLoan (b: Book) (n: Name) (ls: Loan list): Loan list =
    match ls with
    | [] -> []
    | (bx, bn, _)::tail when b=bx && n=bn -> tail
    | l::tail -> l::(removeLoan b n tail);;

(*
5. Declare a function reminders: Date -> Loan list -> (Name * Book) list. The
   value of reminders d0 ls is a list of pairs (n, b) from loans (b, n, d) in ls where d < d0.
   We interpret d < d0 as “date d is before date d0”.

   For example, reminders 3 ls0 has two elements: ("Paul","Programming in Haskell")
   and ("Dick", "Elements of the theory of computation").
*)

let rec reminders (d: Date) (ls: Loan list): (Name * Book) list =
    match ls with
    | [] -> []
    | (b, n, dl)::tail when dl < d -> (n, b)::(reminders d tail)
    | _::tail -> reminders d tail;;

(*
6. In this problem, we consider a textual form of the reminders from Question 5, where,
   for example, a letter reminding Paul to return "Programming in Haskell" has the form:

       "Dear Paul!
        Please return "Programming in Haskell".
        Regards Robin"

   Declare a function toLetters: (Name * Book) list -> string list, that trans-
   forms a list pairs (n, b) to a list of corresponding strings (letters). Notice, the escape
   characters \n and BACKSLASH QUOTES denote newline and citation quotation, respectively.
*)

let rec toLetters (nb: (Name * Book) list): string list =
    match nb with
    | [] -> []
    | (n, b)::tail -> ("Dear " + n + "!\nPlease return \"" + b + "\".\nRegards Robin")::(toLetters tail);;

(*
7. This question should be solved using functions from the List library. You should not
   use explicit recursion in the declarations.
   
   1. Give an alternative declaration of toLetters using List.map.
   2. Give an alternative declaration of reminders using List.foldBack.
*)

let toLetters1 (nb: (Name * Book) list): string list = 
    nb |> List.map (fun (n, b) -> ("Dear " + n + "!\nPlease return \"" + b + "\".\nRegards Robin"));;

let reminders1 (d: Date) (ls: Loan list): (Name * Book) list =
    List.foldBack 
        (fun (b: Book, n: Name, dl: Date) acc -> //compiler so stoopid
            if dl < d then (n, b)::acc 
            else acc) 
        ls
        ([] : (Name * Book) list);;
