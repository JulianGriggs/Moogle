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

exception WrongLogic;;

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

let rec twos : num stream = 
  lazy (
    Cons (two, twos)
  )

let rec fives : num stream = 
  lazy (
    Cons (five, fives)
  )


let a = lazy (Cons(zero, lazy (Cons(two, twos))))
let b = lazy (Cons(one, lazy (Cons(four, fives))))


(*>* Problem 2.1.a *>*)
(* Implement the head and tail functions *)

let head (s:'a stream) : 'a =
  match s with 
	| lazy (Cons (hd,_)) -> hd
;;

assert(head ones = one);;

let tail (s:'a stream) : 'a stream =
  match s with 
	| lazy (Cons (_,tl)) -> tl
;;

let rec test_tail (n:int) =
	match n with 
	| 0 -> ()
	| n -> assert(head (tail ones) = one); test_tail (n-1)
;;

test_tail 100;;

(*>* Problem 2.1.b *>*)
(* Implement map *)

let rec map (f:'a -> 'b) (s:'a stream) : 'b stream =
  match s with 
	| lazy (Cons (hd,tl)) -> lazy (Cons(f hd, map f tl))
;;



(*>* Problem 2.1.c *>*)
(* Define the infinite stream of natural numbers *)
let rec nats : num stream = 
	let rec aux i =
		lazy (
  		Cons (i, aux ((+/) i one))
		)
	in aux zero
;;

(*>* Problem 2.1.d *>*)
(* Write a function nth, which returns the nth element of a
   stream. NOTE: the function nth should be zero-indexed. In other
   words, "nth 0 s" should be equivalent to "head s". *)

let rec nth (n:num) (s:'a stream) : 'a =
	if (=/) n zero then head s else nth ((-/) n one) (tail s)
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

  let rec aux (prev: num option) (s1': num stream) (s2': num stream) = 
    let h1 = head s1' in
    let h2 = head s2' in
    let h1_inFront = lazy (Cons(h1, aux (Some h1) (tail s1') s2')) in
    let h2_inFront = lazy (Cons(h2, aux (Some h2) s1' (tail s2'))) in
    match prev with 
    | None ->
      if (<=/) h1 h2 then h1_inFront
      else                h2_inFront
    | Some x -> 
      if (<=/) h1 h2 then 
        if (=/) x h1 then tail h1_inFront
        else              h1_inFront
      else 
        if (=/) x h2 then tail h2_inFront
        else              h2_inFront
  in

  aux None s1 s2
;;

(*>* Problem 2.1.f *>*)
(* What kinds of inputs cause your "merge" function above to do something
   bad?  What bad thing happens in these cases?  Answer within the string. *)

let p21f = "Inputs that repeat forever cause merge to lead to a 
            stack overflow error.  Example: merge nats ones" ;;


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

type 'a spread_sheet = 'a stream stream;; (* change me! *)

(* you can assume all coordinates given are non-negative *)
type coordinates = num * num ;;

(* a spreadsheet containing all zeros *)
let zeros : num spread_sheet = 
	let rec aux_j: num stream =
		lazy (
			Cons(zero, aux_j)
		)
	in
	let rec aux_i : num stream stream = 
		lazy (
			Cons(aux_j, aux_i)
		)
	in
	aux_i
;; 


(* return the element at the (i,j) coordinate in ss *)
let get ((i,j):coordinates) (ss:'a spread_sheet) : 'a = 
  let rec aux_j n s = 
		if (=/) n zero then head s
		else aux_j ((-/) n one) (tail s)
	in 
	let rec aux_i n ss =
		if (=/) n zero then aux_j j (head ss) 
		else aux_i ((-/) n one) (tail ss)
	in 
	aux_i i ss
;;

(* create a new spreadsheet where the (i,j) element of the spreadsheet
 * contains f i j xij  when xij was the (i,j) element of the input spreadsheet
 *)
let map_all (f:num -> num -> 'a -> 'b) (ss:'a spread_sheet) : 'b spread_sheet = 
	let rec aux_j (i:num) (j:num) (s:'a stream) : 'b stream =
		lazy (
			Cons(f i j (head s), aux_j i ((+/) j one) (tail s))
		)
	in 
	let rec aux_i (i:num) (ss:'a spread_sheet) : 'b spread_sheet = 
		lazy (
			Cons(aux_j i zero (head ss), aux_i ((+/) i one) (tail ss))
		)
	in
	aux_i zero ss
;;

(* create an infinite multiplication table in which every cell contains the
 * product of its indices *)
let multiplication_table : num spread_sheet = 
	let rec aux_j (i:num) (j:num) : num stream =
		lazy (
			Cons(( */ ) i j, aux_j i ((+/) j one))
		)
	in 
	let rec aux_i (i:num) : num spread_sheet = 
		lazy (
			Cons(aux_j i zero, aux_i ((+/) i one))
		)
	in
	aux_i zero
;;

(* produce a spreadsheet in which cell (i,j) contains the ith element
 * of is and the jth element of js *)
let cross_product (is:'a stream) (js:'b stream) : ('a * 'b) spread_sheet =
	let rec aux_j (iv:'a) (js:'b stream) : ('a * 'b) stream =
		lazy (
			Cons((iv, head js), aux_j iv (tail js))
		)
	in 
	let rec aux_i (is:'a stream) (js:'a stream) : ('a * 'b) spread_sheet = 
		lazy (
			Cons(aux_j (head is) js, aux_i (tail is) js)
		)
	in
	aux_i is js
;;

assert(head (head zeros) = zero);;
assert(head (tail (head (tail (zeros)))) = zero);;
assert(get (zero,zero) zeros = zero);;
assert(get (one,one) zeros = zero);;
assert(get (zero,zero) multiplication_table = zero);;
assert(get (one,one) multiplication_table = one);;
assert(get (one,three) multiplication_table = three);;
assert(get (two,two) multiplication_table = four);;
assert(head (head multiplication_table) = zero);;
assert(head (tail (head (tail (multiplication_table)))) = one);;
assert(get (one,one) (map_all (fun i j v -> (+/) v one) zeros) = one);;
assert(get (one,one) (map_all (fun i j v -> (+/) i j) zeros) = two);;
assert(get (one,one) (map_all (fun i j v -> ( */ ) i j) zeros) = one);;
assert(get (three,two) (map_all (fun i j v -> (+/) i v) zeros) = three);;
assert(get (one,one) (cross_product nats nats) = (one,one));;
assert(get (one,two) (cross_product nats nats) = (one,two));;
assert(get (two,two) (cross_product nats nats) = (two,two));;


