(* This file depends on the Timing Module, which depends on the
 * OCaml Unix library.
 *
 * To load this file in the OCaml toplevel, you need to start Ocaml 
 * with unix.cma (the Unix library) as an argument: 
 *
 *   % ocaml unix.cma
 *            Objective Caml version 3.11.0
 *   # #use "timing.ml";;
 *
 * To use the ocamlc compiler, you need to include unix.cma as well.
 * For example:
 *
 * ocamlc -o queue unix.cma timing.mli timing.ml queue.ml
 *)

open Timing ;;
open Lazy ;;

module type QUEUE = sig
  type 'a q

  val emp : 'a q
  val ins : 'a * 'a q -> 'a q
  val rem : 'a q -> ('a * 'a q) option 
end;;

module SlowestQ : QUEUE = struct
  type 'a q = 'a list

  let emp = []

  let ins (i,q) = q @ [i]
 
  let rem q =
    match q with
      [] -> None
    | hd::tail -> Some (hd, tail)
end;;

module SlowQ : QUEUE = struct
  type 'a q = 'a list * 'a list

  let emp = ([], [])

  let ins (i,q) =
    let (front, back) = q in
    (front, i::back)

  let rem q =
    match q with
      ([],[]) -> None
    | (hd::front, back) -> Some (hd, (front,back))
    | ([], back) -> 
       (match List.rev back with
         hd::tail -> Some (hd, (tail, []))
        | _ -> failwith "impossible")       
end;;

module type LAZY_LIST = sig

  (* Notice that this signature hides the fact that             *
   * 'a llist and 'a rep are actually mutually recursive types. *
   * It's not a big deal, but I thought you might find it cool. *
   * It is a feature that we haven't seen before.               *
   * Note also that the details of 'a rep are revealed -- the   *
   * client knows that an 'a rep is a type with a Nil and a     *
   * Cons.  We call such a definition in a signature a          *
   * "transparent" type whereas we call 'a llist an abstract or *
   * "opaque" type.                                             *)

  type 'a llist
  type 'a rep = Nil | Cons of 'a * 'a llist

  val empty : 'a llist
  val cons : 'a -> 'a llist -> 'a llist
  val cat : 'a llist -> 'a llist -> 'a llist
  val rev : 'a llist -> 'a llist
  val reveal : 'a llist ->  'a rep
end;;

module LL : LAZY_LIST = 
struct
  type 'a rep = Nil | Cons of 'a * 'a llist
  and  'a llist = 'a rep Lazy.t

  let empty = lazy(Nil)

  let cons i ll = lazy(Cons (i,ll))

  (* concatenate two lazy lists *)
  let rec cat xs ys =
    match force xs with
	Nil -> ys
      | Cons (hd,tail) -> cons hd (cat tail ys)

  (* reverse a lazy list *)
  let rev xs =
    let rec aux xs reversed =
    match force xs with
	Nil -> reversed
      | Cons (hd,tail) ->  aux tail (cons hd reversed)
    in
    aux xs empty

  let reveal xs = force xs
end;;

(* Any sequence of operations in this implementation should *
 * execute in amortized constant time                       *)

module AmortizedQ : QUEUE = struct

  (* (lenf,front,lenb,back) represents a queue *)
  type 'a q = int * 'a LL.llist * int * 'a LL.llist

  (****** DO THIS *****************************************************)
  (* the function rep_inv should implement the representation invariant
   * for amortized queues.  It should check as many of the invariants of
   * lists as possible. When writing rep_inv, pay special attention to
   * the fact that the check function is called at the end of insert
   * and at the end of rem.  What representation invariant does it 
   * help to ensure?  *)
  let rep_inv (lenf,front,lenb,back) = failwith "unimplemented";; 

  let emp = (0, LL.empty, 0, LL.empty)

  let check q =
    let (lenf,front,lenb,back) = q in
    if lenb <= lenf then q 
    else (lenf+lenb, LL.cat front (LL.rev back), 0, LL.empty)

  let ins (i,q) = 
    let (lenf,front,lenb,back) = q in
    check (lenf, front, lenb+1, LL.cons i back)

  let rem q = 
    let (lenf,front,lenb,back) = q in
    match LL.reveal front with
	LL.Nil -> None
      | LL.Cons (hd,tail) -> 
	let q' = check (lenf-1,tail,lenb,back) in
	Some (hd, q')

end;;

(* Performance analysis *)

module type PERF = sig
  val test1 : int -> float
  val test2 : int -> float
end;;

module Performance (Q:QUEUE) : PERF = struct

  (* test1:
   *
   * Executes a series of n queue operations and returns time taken.
   * You do not have to execute *exactly* n -- you can be off by 1 or 2.
   * We are looking for *ballpark* trend lines in performance.
   * 
   * You should use the functions in the Timing module to help you.
   * Please see the interface file timing.mli
   *
   * The time taken to run test1 n should be ~c*n for some constant c 
   * when the Q in question is either SlowQ or AmortizedQ.
   *
   * The time taken to run test1 n should be ~c*n^2 when the Q is SlowestQ.
   *
   * In other words, you need to design a test that makes SlowestQ look
   * slow (quadratic) and the other two implementations fast (linear).
   *
   * EXPLAIN WHAT YOUR TEST DOES IN A COMMENT
   *)
  let test1 (n:int) = 0.0;;

  (* test2:
   *
   * Executes a series of n queue operations and returns time taken.
   * You do not have to execute *exactly* n -- you can be off by 1 or 2.
   * We are looking for *ballpark* trend lines in performance.
   *
   * You should use the functions in the Timing module to help you.
   * Please see the interface file timing.mli
   *
   * The time taken to run test2 n should be ~c*n for some constant c 
   * when the Q in question is AmortizedQ.
   *
   * The time taken to run test2 n should be ~c*n^2 when 
   * the Q is either SlowQ or Slowest.
   *
   * In other words, you need to design a test that makes SlowQ and 
   * SlowestQ both look slow (quadratic) and but AmortizedQ look fast 
   * (linear).
   *
   * EXPLAIN WHAT YOUR TEST DOES IN A COMMENT
   *)
  let test2 (n:int) = 0.0;;
end;;

module SlowestP = Performance(SlowestQ);;
module SlowP = Performance(SlowQ);;
module AmortizedP = Performance(AmortizedQ);; 

let print_header () =
  print_string "-----  -------- Test 1 ---------   -------- Test 2 ---------\n";
  print_string "  N    Slowest   Slow  Amortized   Slowest   Slow  Amortized\n";
  print_string "-----  -------------------------   -------------------------\n"
;;

let print_row n t1slowest t1slow t1amortized t2slowest t2slow t2amortized =
  let space () = print_string "  " in
  let float f = Printf.printf "%6.4f" f in
  if n < 10000 then print_string " ";
  print_int n;       space ();
  float t1slowest;   space ();
  float t1slow;      space ();
  float t1amortized; space ();
  space ();
  space ();
  float t2slowest;   space ();
  float t2slow;      space ();
  float t2amortized; print_newline()
;;

let experiment (n:int) : unit =
  print_row n
    (SlowestP.test1 n)
    (SlowP.test1 n)
    (AmortizedP.test1 n)
    (SlowestP.test2 n)
    (SlowP.test2 n)
    (AmortizedP.test2 n) 
;;

let main () = 
  (* you may change these numbers to suit your machine *)
  (* Recall that if you double the input size and your algorithm is
   * linear then you should see an approximate doubling of the time taken
   * If your algorithm is quadratic, you should see an approximate 
   * quadrupling of the time taken  *)
  let ns = [1000; 2000; 4000; 8000; 16000; 32000] in
  print_header();
  List.iter experiment ns
;;

(*

(* uncomment this block to run tests, 
 * but please do not submit with it uncommented
 *)
main ();;
*)
