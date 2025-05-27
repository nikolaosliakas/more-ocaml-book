
(* No.1
    arg1 - expenses - list of ints
    arg2 - budget - int 
*)
let deduct_expenses exp bud =
  List.fold_left (-) bud exp

(* No.2
  list len using fold_
*)
let list_len l =
  List.fold_left (fun acc _ -> acc + 1) 0 l

(* No.3
  Last element of list!
  Answer from ChatGPT
*)
let find_last_e l =
  List.fold_right 
    (fun x acc -> match acc with None -> Some x | Some _ -> acc) 
    l 
    None 
(*Answer from the Textbook!*)
let last l =
  match l with
    [] -> None
  | _ -> Some (List.fold_left (fun _ e -> e) (List.hd l) l)

(* No. 4
Reverse a list using fold
*)
let list_reverse l =
  List.fold_left (fun acc x -> x :: acc) [] l
(* No. 5
Write a version of List.mem with a fold_ func!
*)
let rec find_element x l =
  match l with 
  [] -> false
 | h::t -> if h = x then true else find_element x t

let find_element_stuff x l =
  List.fold_left 
  (fun acc y -> if x = y then acc = true else acc = false) 
  false 
  l
(*Textbook answer for 5*)
let member x l =
  List.fold_left (fun a e -> e = x || a) false l

(*No. 6 
arg1 - non-empty string list of words
concatenate with spaces delimiting - comment on efficiency *)

let concat_words l =
  List.fold_left 
    (fun a e -> 
      match a with 
      "" -> e
      | _ -> a ^ " " ^ e) 
    "" 
    l
(*No. 7
use fold_tree to calc 'depth'
*)
type 'a tree =
  Lf
  | Br of 'a * 'a tree * 'a tree

(* ('a -> 'b -> 'b -> 'b) -> 'b -> 'a tree -> 'b *)
let rec fold_tree f e t =
  match t with
    Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

(*from textbook  - max adds either another depth of 1 or 0 if tree is empty*)
let max_depth l =
  fold_tree (fun _ l r -> 1 + max l r) 0 l

(* No. 8
#
*)