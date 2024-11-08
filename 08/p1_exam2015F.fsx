(*
We consider the use of appliances (in Danish ‘husholdningsapparater’) like washing machines,
dishwashers and coffee machines. A usage of an appliance a is a pair (a, t), where
t is the time span (in hours) the appliance is used. A usage list is a list of the individual
usages during a full day, that is, 24 hours. This is modelled by:
*)

  type Appliance = string
  type Usage = Appliance * int

  let ad0 = ("airfryer", 10)
  let ad1 = ("washing machine", 2)
  let ad2 = ("coffee machine", 1)
  let ad3 = ("dishwasher", 2)
  let ad4 = ("portal", -1)
  let ats = [ad1; ad2; ad3; ad1; ad2]

  let ats2 = [ad1; ad2; ad3; ad3; ad0; ad0; ad0] //non-WF
  let ats3 = [ad4; ad1; ad3; ad3] //non-WF
(*
where ats is a value of type Usage list containing one usage of the dishwasher and two
usages of the washing machine and the coffee machine.
*)

(*
1. Declare a function: inv: Usage list -> bool, that checks whether all time spans
   occurring in a usage list are positive
*)
//assuming 0 usage is not favored

let inv (xs: Usage list) : bool = xs |> List.filter (fun (_, b) -> b <= 0) |> List.isEmpty;;

(*
2. Declare a function durationOf: Appliance -> Usage list -> int, where the value
   of durationOf a ats is the accumulated time span appliance a is used in the list ats.

   For example, durationOf "washing machine" ats should be 4.
*)

let durationOf (a: Appliance) (ats: Usage list) : int =
    ats |> List.filter (fun (a1,_) -> a=a1) |> List.fold (fun acc (_, d) -> acc+d) 0

(*
3. A usage list ats is well-formed if it satisfies inv and the accumulated time span of any
   appliance in ats does not exceed 24. Declare a function that checks this well-formedness
   condition.
*)
let isWF ats = 
    match inv ats with
    | false -> false;
    | true -> ats 
              |> List.map (fun (x,y) -> durationOf x ats) 
              |> List.filter (fun x -> x > 24) 
              |> List.isEmpty;;

(*
4. Declare a function delete(a, ats), where a is an appliance and ats is a usage list.
   The value of delete(a, ats) is the usage list obtained from ats by deletion of all us-
   ages of a. For example, deleting usage of the coffee machine from ats should give
   [ad1; ad3; ad1]. State the type of delete.
*)
//the type of delete is Appliance -> Usage list -> Usage list

let delete (a: Appliance) (ats: Usage list) : Usage list =
    ats |> List.filter (fun (x,_) -> x <> a)

(*
We now consider the price of using appliances. This is based on a tariff mapping an
appliance to the price for one hour’s usage of the appliance:
*)

type Price = int
type Tariff = Map<Appliance, Price>

let trf1 = Tariff [("washing machine", 5);("coffee machine",1);("dishwasher",4)]

(*
5. Declare a function isDefined ats trf , where ats is a usage list and trf is a tariff. The
   value of isDefined ats trf is true if and only if there is an entry in trf for every appliance
   in ats. State the type of isDefined.
*)
//the type of isDefined is Usage list -> Tariff) -> bool

let isDefined (ats: Usage list) (trf: Tariff) : bool =
    ats |> List.filter (fun (x,_) -> not (Map.containsKey x trf)) |> List.isEmpty;;

(*
6. Declare a function priceOf: Usage list -> Tariff -> Price, where the value of
   priceOf ats trf is the total price of using the appliances in ats. The function should
   raise a meaningful exception when an appliance is not defined in trf .
*)

exception ApplianceNotDefinedError of string

let priceOf (ats: Usage list) (trf: Tariff) : Price =
    match (isDefined ats trf) with
    | false -> raise (ApplianceNotDefinedError("You are doomed"))
    | true -> ats |> List.fold (fun acc (x,y) -> acc + y * (Map.find x trf)) 0;;
