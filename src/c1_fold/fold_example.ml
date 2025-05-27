let rec fold_left f a l =
  match l with
   [] -> a
   | h::t -> fold_left f (f a h) t

let rec fold_right f l a =
  match l with
  [] -> a
  | h::t -> f h (fold_right f t a)

let sum_of_intlist l =
  fold_left (+) 0 l

(* List to set by using the accumulator to 'check' using List.mem*)

let setify l =
  fold_left (fun a e -> if List.mem e a then a else e :: a)
            []
            l
(*
# setify [1;2;3;4;4;4;5;5];;
- : int list = [5; 4; 3; 2; 1]
*)

(* fold_right is not tale recursive - the intermediate expression is 
proportional to the size of its input*)

let rec map_naive f l = 
  match l with
  [] -> []
  | h :: t -> f h :: map_naive f t

let map f l =
  fold_right (fun e a -> f e :: a) l []
(* to make the fold_right tail-recursive fold left but with the cost of a reversal*)
let fold_right_left f l e =
  fold_left (fun x y -> f x y) e (List.rev l)

(*Copy and append are fold right!*)
let copy l =
  fold_right (fun e a -> e :: a) l []
let append x y =
  fold_right (fun e a -> e :: a) x y

let split l =
  fold_right
    (fun (x,y) (xs, ys) -> (x :: xs, y :: ys))
    l
    ([], [])
(*
split [(1, "one"); (2, "two")];;
- : int list * string list = ([1; 2], ["one"; "two"])
*)

(* TREES--- definition for binary can be a tree fold*)

type 'a tree =
  Lf
  | Br of 'a * 'a tree * 'a tree

(* ('a -> 'b -> 'b -> 'b) -> 'b -> 'a tree -> 'b *)
let rec fold_tree f e t =
  match t with
    Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let tree_size t = fold_tree (fun _ l r -> 1 + l + r) 0 t
let tree_sum t = fold_tree (fun x l r -> x + l + r) 0 t

(*  Tree traversal!*)

let tree_preorder t = fold_tree (fun x l r -> [x] @ l @ r) [] t
let tree_inorder t = fold_tree (fun x l r ->   l @ [x] @ r) [] t
let tree_postorder t = fold_tree (fun x l r ->  l @ r @ [x] ) [] t