
(* Records are named tupes with {} bracketting *)

type point = {x : float; y : float; label:string}

let p = {x=4.2; y=5.9; label="point_of_no_return"}

(* 
Records can be parameterized lika list or variant dtype
This allows for an int point or string point etc.
*)

type 'a point ={
  x : float; 
  y : float; 
  label:string;
  content: 'a
}

let p1 = {
  x=4.2; y=5.9; label="point_of_no_return"; content=[5;6;7]
}

let p2 = {
  x=4.2; y=5.9; label="point_of_no_return"; content=[|8;9;10|]
}

let make_point x y l c =
  {x = x; y= y; label = l; content=c}
(* You can do the same without the assignment if field names and args are the same
  as the type definition *)

let make_point1 x y label content =
  {x; y; label; content}

(* 
Pattern matching for records
For functions that take a record type as argument you can use the pattern match
to access only what you need
*)

let string_of_point {label; x; y; _} =
  label
  ^ " = ("
  ^ string_of_float x
  ^ ", "
  ^ string_of_float y
  ^ ")"

(* Above and below are equivalent - but you should use 
the one with the wildcard as it is more explicit when red
*)

let string_of_point {label; x; y} =
  label
  ^ " = ("
  ^ string_of_float x
  ^ ", "
  ^ string_of_float y
  ^ ")"

(* 
=======================================
Mutability and record manipulation
=======================================
*)

(* One or more field changed  
- with *)
let relabel p l = {p with label =l}
let relabel p label = {p with label}

(* Reflect a point about the line x=y *)

let mirror p = {p with x =p.y; y=p.x}

(* Mutable mut *)

type 'a mut_point ={
  x : float; 
  y : float; 
  label:string;
  mutable content: 'a
}
(* NB - a reference type is just a record with a single mutable field *)

let p1 = {
  x=4.2; y=5.9; label="point_of_no_return"; content=[5;6;7]
}

(* val p1 : int list mut_point =
  {x = 4.2; y = 5.9; label = "point_of_no_return"; content = [5; 6; 7]}
   # p1.content <- [11;12;13];;
- : unit = ()
# p1;;
- : int list mut_point =
{x = 4.2; y = 5.9; label = "point_of_no_return"; content = [11; 12; 13]}
  *)






