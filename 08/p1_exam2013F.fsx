(*
A multiset (or bag) is a generalization of a set, where an element e is associated with
  a multiplicity, ie.e the number of times e occurs in the multiset. 

We shall represent a finite multiset ms by a list of pairs [(e_1,n_1);...;(e_k,n_k)],
  where a member (e_i,n_i) represents that e_i is a  member of ms with multiplicity
  n_i, i.e. e_i occurs n_i times in ms.

For a representation [(e_1, n_1);...;(e_k,n_k)] of a multiset we require that every
  multiplicity n_i is positive, and that the elements are distinct, i.e. e_i<>e_j, for i<>j.

This property is called the multiset invariant. A consequence of this is that the empty
  multiset is represented by the empty list.

We shall use the type Multiset<'a> declared as follows:
*)

type Multiset<'a when 'a : equality> = ('a * int) list

(*
For example [("b", 3); ("a",5); ("d",1)] has type Multiset<string> and represents the
multiset with 3 occurrences of "b", 5 of "a" and 1 of "d".

1. Declare a function inv: Multiset<'a> -> bool such that inv(ms) is true when ms
satisfies the multiset invariant.
*)

let rec invH (acc: ('a list)) (xs: Multiset<'a>) = 
    match xs with
    | []                                 -> true
    | (_, y)::_ when y<=0                -> false
    | (x, _)::_ when List.contains x acc -> false
    | (x, y)::tail                       -> invH (x::acc) tail;;

let rec inv xs = invH [] xs;;


(*
In your solutions to the below questions, you can assume that multisets occurring in
  arguments satisfy the multiset invariant, and the declared functions must preserve this property,
  i.e. results must satisfy this invariant as well.

2. Declare a function insert: 'a -> int -> Multiset<'a> -> Multiset<'a>, where insert e n ms
     is the multiset obtained by insertion of n occurrences of the element e in ms.

   For example: insert "a" 2 [("b", 3); ("a",5); ("d",1)] will result in a multiset
   having 7 occurrences of "a".
*)

let rec insert e n (ms: Multiset<'a>) : Multiset<'a> =
    match ms with
    | [] -> [(e, n)]
    | (x, y)::tail when x=e -> (x, y+n)::tail
    | xp::tail -> xp::(insert e n tail)

(*
3. Declare a function numberOf, where numberOf e ms is the multiplicity (i.e. the number of occurrences)
   of e in in the multiset ms. State the type of the declared function.
*)

let rec numberOf e (ms: Multiset<'a>) : int =
    match ms with
    | [] -> 0
    | (x, y)::_ when e=x -> y
    | _::tail            -> numberOf e tail

(*
4. Declare a function delete, where delete e ms is the multiset obtained frm ms by delettion
   of one occurrence of the element e.
*)

let rec delete e (ms: Multiset<'a>) : Multiset<'a> =
    match ms with
    | [] -> []
    | (x, 1)::tail when e=x -> tail
    | (x, y)::tail when e=x -> (x, y-1)::tail
    | xp::tail -> xp::(delete e tail)

(*
5. Declare a function union: Multiset<'a> * Multiset<'a> -> Multiset<'a>, for making the union
   of two multisets. This function generalizes the union function on sets in a natural way
   taking multiplicities in to account, e.g. the result of

   union ([("b",3); ("a",5); ("d",1)], [("a",3); ("b",4);("c",2)])

   is the multiset containing 8 occurrences of "a", 7 of "b", 2 of "c" and 1 of "d".
*)

let rec union (xs: Multiset<'a>, ys: Multiset<'a>): Multiset<'a> =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | xs, (y1, y2)::ytail -> union ((insert y1 y2 xs), ytail);;

(*
We shall now represent multisets by maps from elements to multiplicities:
*)

type MultisetMap<'a when 'a : comparison> = Map<'a, int>;;

(*
This representation of a mutliset ms has a simpler invariant: the multiplicity n of each entry
  (e,n) of ms satisfies n > 0.

6. Give new declerations for inv, insert and union on the basis of the map representation.
*)

let inv1 (ms: MultisetMap<'a>): bool =
    Map.forall (fun _ v -> (v > 0)) ms;;

let insert1 e n (ms: MultisetMap<'a>) : MultisetMap<'a> =
    match (Map.containsKey e ms) with
    | false -> Map.add e n ms
    | true -> Map.change e (fun x -> match x with
                                     | Some s -> Some (s + n)
                                     | None -> None
                           ) ms

let insert2 e n (ms: MultisetMap<'a>) : MultisetMap<'a> =
    Map.change e (function
        | None -> Some n
        | Some x -> Some (x + n)
    ) ms

let f acc k v = insert2 k v acc //insert the key value pair into acc

let union2 (xs: MultisetMap<'a>, ys: MultisetMap<'a>): MultisetMap<'a> =
    match xs, ys with
    | xs, ys when Map.isEmpty xs -> ys
    | xs, ys when Map.isEmpty ys -> xs
    | xs, ys -> Map.fold f xs ys;; // for each key-value pair in ys insert it into xs





