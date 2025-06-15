type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec lseq n =
    Cons (n, fun () -> lseq (n + 1))

(* Only one cconstructor in our dtype so we can pattern match 
directly in the argument to the function.
*)
let lhd (Cons (n, _)) = n

(* The unit in the function body forces evaluation of the constructor tail portion *)
let ltl (Cons (_, tf)) = tf ()

(* ================================= *)
(* ===Lazy take and drop=========== *)
(* ================================= *)

(* Eager Take *)

let rec etake n l =
    match n, l with
    | (0, _)| (_, []) -> []
    | (_, h::t) -> h :: etake (n-1) t

(* Lazy Take 
    yields a normal list. Keep evaluating reducing n until 0.
*)
let rec ltake (Cons(h, tf)) n =
    match n with
    |0 -> []
    |_ -> h :: ltake (tf ()) (n-1)

(*
# ltake (lseq 0) 20;;
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19]
*)

(* Eager Drop *)
let rec edrop n l =
    match n, l with
    | (0, _)| (_, []) -> []
    | (_, _::t) -> edrop (n-1) t

(* Lazy Drop *)
let rec ldrop (Cons (h, tf) as ll) n =
    match n with 
    | 0 -> ll
    | _ -> ldrop (tf ()) (n - 1)
(*
# ldrop (lseq 0) 20;;
- : int lazylist = Cons (20, <fun>)
*)

(* ================================= *)
(* ===Map and Filter=========== *)
(* ================================= *)

(* Eager Map *)
let rec emap f l =
    match l with
    | [] -> []
    | h::t -> f h :: emap f t
(* Lazy Map *)
let rec lmap f (Cons (h, tf)) =
    (* Returns a lazylist which means it only computes the head element
    the evaluation of the rest of the elements is delayed
    *)
    Cons (f h, fun () -> lmap f (tf ()))

(* Eager Filter *)
let rec efilter f l =
    match l with 
    [] -> []
    |h::t -> if (f h) then h :: efilter f t else efilter f t

(* Lazy Filter *)
(* Warning - lazy filter will continue until it finds a head that evaluates 
to true when the function is applied. Otherwise it will not return.
*)
let rec lfilter f (Cons(h, tf)) = 
    if f h then
        Cons (h, fun () -> lfilter f (tf ()))
    else
        lfilter f (tf ())


(* ================================= *)
(* === Number Fun =========== *)
(* ================================= *)

(* Cubes divisible by five *)
let cubes =
    (* Apply filter to sequence evaluated*)
    lfilter
        (fun x -> x mod 5 = 0)
        (* Calculate cubes from one incrementing with lazy sequence by 1
        each iteration.
        *)
        (lmap (fun x -> x * x * x) (lseq 1))

(*
Use 'ltake' to get the first 20 elements from cubes

# ltake cubes 20;;
- : int list =
[125; 1000; 3375; 8000; 15625; 27000; 42875; 64000; 91125; 125000; 166375;
 216000; 274625; 343000; 421875; 512000; 614125; 729000; 857375; 1000000]
*)

(* Make primes starting from 2
two functions 
*)

let rec mkprimes (Cons (h, tf)) =
    Cons (h, fun () -> 
            mkprimes (lfilter (fun x -> x mod h <> 0) (tf ()))
        )
let primes = mkprimes (lseq 2)
(* # ltake primes 20;;
- : int list =
[2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71] *)

(* ==== Not Lazy-listable =========== *)
(* ================================= *)

(* You cannot reverse a lazy list
nor append two lazy lists

*)


(* Appending lazy lists is done through
            INTERLEAVING
Very cool - constantly swapping out the arguments to parse the lazylist each time.
*)

let rec interleave (Cons (h, tf)) l =
    Cons (h, fun () -> interleave l (tf ()))

(* Interleave 0 and 1
a function to evaluate constant lists
*)
let rec lconst n =
    Cons (n, fun () -> lconst n)
let rec interleaved = 
interleave (lconst 0) (lconst 1)

(* To make twenty interleaves of 0 and 1
ltake interleaved 20;;
- : int list = [0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1]
*)

(* VERY cool try to see the permiutations of 0 1 *)

let rec allfrom l =
    Cons (l, fun () -> 
    interleave (allfrom (0::l)) (allfrom (1::l)))
let allones = allfrom [];;

(* 
                []
            \       \
            [0]     [1]
        \       \      \    \
        [0;0]   [1;0]   [0;1]   [1;1]

ltake allones 20;;
- : int list list =
[[]; [0]; [1]; [0; 0]; [0; 1]; [1; 0]; [1; 1]; [0; 0; 0]; [0; 0; 1]; 
 [0; 1; 0]; [0; 1; 1]; [1; 0; 0]; [1; 0; 1]; [1; 1; 0]; [1; 1; 1];
 [0; 0; 0; 0]; [0; 0; 0; 1]; [0; 0; 1; 0]; [0; 0; 1; 1]; [0; 1; 0; 0]]
    
*)