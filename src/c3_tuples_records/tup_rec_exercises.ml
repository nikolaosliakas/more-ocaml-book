
(* 
======================================
Q1 - Update reference without := operator
======================================
*)

(* 
# let x = ref 10;;
val x : int ref = {contents = 10}
# x.contents <- 20;;
- : unit = ()
# x;;
- : int ref = {contents = 20}
*)

(* 
======================================
Q2 - Using "Time Functions sec" of Unix module
program "It is 2:45 on Wednesday 8 January 2014"
https://ocaml.org/manual/5.3/api/Unix.html#VALtime
======================================
*)
open Sys
open Unix

let sub_wday i =
  match i with
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | 7 -> "Sunday"
  | _ -> "Not_in_week"
let sub_month i =
  match i with
  | 0 -> "January"
  | 1 -> "February"
  | 2 -> "March"
  | 3 -> "April"
  | 4 -> "May"
  | 5 -> "June"
  | 6 -> "July"
  | 7 -> "August"
  | 8-> "September"
  | 9 -> "October"
  | 10 -> "November"
  | 11 -> "December"
  | _ -> "Not_in_week"
let time_time =
  Unix.localtime (Unix.time ())

let get_time_string = 
  string_of_int (time_time).tm_hour
  ^
  ":"
  ^
  string_of_int (time_time).tm_min

let get_date_string =
  sub_wday (time_time).tm_wday
  ^ " " ^
  string_of_int (time_time).tm_mday
  ^ " " ^
  sub_month (time_time).tm_mon
  ^ " " ^
  string_of_int ((time_time).tm_year + 1900)


(*
# let make_display_date_record =
  Unix.gmtime (Unix.time ());;
val make_display_date_record : Unix.tm =
  {Unix.tm_sec = 5; tm_min = 19; tm_hour = 7; tm_mday = 16; tm_mon = 5;
   tm_year = 125; tm_wday = 1; tm_yday = 166; tm_isdst = false}
*)

(* Incorrect implementation - I am using the Unix module :P
let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r;;

let get_date_from_shell cmd =
  List.hd (run cmd) *)
let print_current_date = 
  
  "It is "
  ^ 
  get_time_string

  ^
  " on "

  ^
  get_date_string
(* 
======================================
Q3 - Difference between:
a. type t = {x : int ref}
b. type t = {mutable x : int}
======================================
*)

(* a has a field x that is an int ref. The reference type
value itself is a record with one mutable field called conents
b has a mutable field named x of type int
  - altering the b requires the '<-' operator
  - altering the value of a requires ':=' assignment
*)
(* 
======================================
Q4 - Record 6 items a...f
a-b
c-d
e-f
======================================
*)
type my_rec = {
  a:int;
  b:int;
  c:string;
  d:string;
  e:float;
  f:float
}

(* 
======================================
Q5 - OCaml Gc - Garbage collector
(a) write summary of GC state
(b) alter the verbosity of the gc with 'control' record
======================================
*)

open Gc

let gc_file = "gc_output.txt"
let write_gc_to_file () =

  let gc_open = open_out gc_file in
    Gc.print_stat gc_open;
    close_out gc_open
