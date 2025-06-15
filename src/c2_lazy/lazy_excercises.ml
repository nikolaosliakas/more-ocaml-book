
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)
(* ===================================================== *)
(* 1. write lazy list that doubles from 1 *)
(* ===================================================== *)

let rec ldoubleseq n =
    Cons (n, fun () -> ldoubleseq (n + n))
(*
ltake (ldoubleseq 1) 20;;
- : int list =
[1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024; 2048; 4096; 8192; 16384; 32768;
 65536; 131072; 262144; 524288]
 *)
 (* ===================================================== *)
(* 2. Return the n-th element of a lazy list where element zero is the head of that list*)
(* ===================================================== *)

let rec n_lelem (Cons (h, tf)) n = 
  match n with
  0 -> h
  |_ -> n_lelem (tf ()) (n-1)

 (* ===================================================== *)
(* 3. 'a list -> 'a lazylist that repeats list sequence *)
(* ===================================================== *)

(* From the author *)

let rec lrepeating_inner c l =
  match c with
  | [] -> raise (Invalid_argument "lrepeating empty list")
  (* Last element - refill the c with the original list *)
  | [x] -> Cons (x, fun () -> lrepeating_inner l l)
  | h::t -> Cons (h, fun () -> lrepeating_inner t l)
let lrepeating l = lrepeating_inner l l
(* 
# ltake (lrepeating [1;2;3]) 15;;
- : int list = [1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3] *)

 (* ===================================================== *)
(* 4. fibonnaci sequence first two elements are 0 and 1 *)
(* ===================================================== *)

let rec fibonnaci_seq x y =
  Cons (y, fun () -> fibonnaci_seq y (x + y))
(*
ltake (fibonnaci_seq 0 1) 10;;
- : int list = [1; 1; 2; 3; 5; 8; 13; 21; 34; 55]
*)

 (* ===================================================== *)
(* 5. Unleave that undos an interleaving 
'b lazylist 0,2,4,6...
'c lazylist 1,3,5,7...
  'a lazylist -> ('b lazylist, 'c lazylist) *)
(* ===================================================== *)

let rec interleave (Cons (h, tf)) l =
    Cons (h, fun () -> interleave l (tf ()))

(* From the author *)
let rec unleave (Cons (h, tf)) =
  (*Variable of lazylist is __evaluated__ from original lazylist
   Then second lazy list from the subsequent element in the initial lazy list
   see - h' is retained for first 0 indexed lazylist

   Then when the pair is constructed:
    h 0 indexed goes to the first pair
    h' is the derived from the let expression from the evaluation of tf ()

    fst and snd are built in to get the first and second components of a pair.
  *)
  let Cons (h', tf') = tf () in
   let t = tf' () in
    (Cons (h, fun () -> fst (unleave t)),
    Cons (h', fun () -> snd (unleave t))
    )
 (* ===================================================== *)
(* 5. Create the lazylist list 
A, B, C, D .... Z
AA, AB, AC, AD,...,AZ
*)
(* ===================================================== *)