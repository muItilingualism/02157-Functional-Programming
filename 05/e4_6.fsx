(*
Make a revised version of the cash register example in Section 4.6 where
*)

type ArticleCode = string;;
type ArticleName = string;;

type Price = int;;


type Register = (ArticleCode * (ArticleName*Price)) list;;

type NoPieces = int;; // np where np >= 0
type Item = NoPieces * ArticleCode;;
type Purchase = Item list;;

type Info = NoPieces * ArticleName * Price;;
type Infoseq = Info list;;
type Bill = Infoseq * Price;;

(*
findArticle: ArticleCode -> Register -> ArticleName * Price
1. The function findarticle is replaced by an application of List.tryFind
*)
let findArticle ac r =
    match List.tryFind (fun (ac', _) -> ac=ac') r with
    | Some (_, anp) -> anp
    | None -> failwith(ac + " is an unknown article code");;


(*
makeBill: Register -> Purchase -> Bill
2. The function makeBill is declared using List.foldBack
*)

let makeBill reg pur =
    List.foldBack 
        (fun (np, ac) (billt1, sumt1) ->
            let (aname, aprice) = findArticle ac reg
            let tprice = np*aprice
            ((np, aname, tprice)::billt1, tprice+sumt1)
        ) pur ([], 0);;
