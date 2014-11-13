(***************** Using the Lazy module ******************)
(* Here we provide an alternate implementation of streams using
   OCaml's lazy module. We recommend that you explore the
   documentation at
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html

   In this portion, you will be reimplementing various functions that
   were defined in class and several more ...
*)

(***************** Using the Num module ******************)
(* This file uses OCaml Num library for arbitrary precision arithmetic.
 * All functions that you write where you might be inclined to use the 
 * simple int type as the type of stream should instead use the num type
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Num.html
 *
 * To load this file in the OCaml toplevel, you need to load the 
 * Num library. The .ocamlinit file distributed with this code does this 
 * for you. You could, alternately, start Ocaml with nums.cma as an argument: 
 *
 *   % ocaml nums.cma
 *
 * To use ocamlbuild, see the associated Makefile.
 *
 * Some useful operators from the num library include:
 *  (+/) addition on num
 *  (-/) subtraction on nums
 *  ( */ ) multiplication on nums -- watch the spaces or you start a comment
 *  (//) division on nums
 *  ( **/ ) power on nums
 *
 * See the documentation for more.
 *)

open Num;;

open Lazy;;

(* some useful numbers *)
let zero  : num = Int 0
let one   : num = Int 1
let two   : num = Int 2
let three : num = Int 3
let four  : num = Int 4
let five  : num = Int 5

type 'a str = Cons of 'a * 'a stream
and 'a stream = 'a str Lazy.t

(* a stream of ones *)
let rec ones : num stream = 
  lazy (
    Cons (one, ones)
  )


(*>* Problem 2.1.a *>*)
(* Implement the head and tail functions *)

let head (s:'a stream) : 'a =
  raise (Failure "unimplemented")
;;

let tail (s:'a stream) : 'a stream =
  raise (Failure "unimplemented")
;;

(*>* Problem 2.1.b *>*)
(* Implement map *)

let rec map (f:'a -> 'b) (s:'a stream) : 'b stream =
  raise (Failure "unimplemented")
;;

(*>* Problem 2.1.c *>*)
(* Define the infinite stream of natural numbers *)
let rec nats : num stream = raise (Failure "unimplemented") ;;

(*>* Problem 2.1.d *>*)
(* Write a function nth, which returns the nth element of a
   stream. NOTE: the function nth should be zero-indexed. In other
   words, "nth 0 s" should be equivalent to "head s". *)

let rec nth (n:num) (s:'a stream) : 'a =
  raise (Failure "unimplemented")
;;

(*>* Problem 2.1.e *>*)
(* Now suppose we have two num streams s1 and s2 sorted in ascending
   order. We wish to merge these into a single stream s such that s is
   sorted in ascending order AND s HAS NO DUPLICATES. Implement this
   function TO THE EXTENT POSSIBLE --- in other words, it should return
   a good answer in most cases, but you'll run in to an issue in certain
   cases.  Use the next question to document any failure modes or 
   incompletenesses or unusual aspects of your function.
*)

let merge (s1:num stream) (s2:num stream) : num stream =
  raise (Failure "unimplemented")
;;

(*>* Problem 2.1.f *>*)
(* What kinds of inputs cause your "merge" function above to do something
   bad?  What bad thing happens in these cases?  Answer within the string. *)

let p21f = "" ;;


(*>* Problem 2.2 *>*)

(* Define a type for an infinite spreadsheet full of cells with type 'a. 
 *
 * You are free to use any representation of infinite spreadsheets you choose.
 *
 * Each cell in the spreadsheet will have coordinates (i,j).  You can think
 * of a cell with coordinates (i,j) as the cell inhabiting the ith row and
 * jth column of the spreadsheet.  You can assume that (i,j) are both
 * non-negative.  Indices in the spreadsheet start at 0.

 * Coordinates will be represented using OCaml's arbitrary precision Num
 * library, as in the rest of this file.

 * Such a spreadsheet will need to support the following operations:
 *)

type 'a spread_sheet = num;; (* change me! *)

(* you can assume all coordinates given are non-negative *)
type coordinates = num * num ;;

(* a spreadsheet containing all zeros *)
let zeros : num spread_sheet = num_of_int 0;; (* change me *)

(* return the element at the (i,j) coordinate in ss *)
let get ((i,j):coordinates) (ss:'a spread_sheet) : 'a = 
  failwith "unimplemented"
;;

(* create a new spreadsheet where the (i,j) element of the spreadsheet
 * contains f i j xij  when xij was the (i,j) element of the input spreadsheet
 *)
let map_all (f:num -> num -> 'a -> 'b) (ss:'a spread_sheet) : 'b spread_sheet = 
  failwith "unimplemented"
;;

(* create an infinite multiplication table in which every cell contains the
 * product of its indices *)
let multiplication_table = num_of_int 0;; (* change me *)

(* produce a spreadsheet in which cell (i,j) contains the ith element
 * of is and the jth element of js *)
let cross_product (is:'a stream) (js:'b stream) : ('a * 'b) spread_sheet =
  failwith "unimplemented"
;;

