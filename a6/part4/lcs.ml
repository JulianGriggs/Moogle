open Memoizer
open Timing

type base = Base.base;;
type dna = Base.dna;;

(* slow lcs *)
let rec slow_lcs ((s1,s2) : dna * dna) : dna =
  match (s1,s2) with 
      ([], _) -> []
    | (_, []) -> []
    | (x :: xs, y :: ys) ->
      if Base.eq x y then
	x :: slow_lcs (xs, ys)
      else
	Base.longer_dna_of (slow_lcs (s1, ys)) (slow_lcs (xs, s2))
;;

(* A potentially useful module *)
module DnaPairOrder : Map.OrderedType with type t = dna * dna =
struct
    type t = dna * dna

    let rec compare_dna x' y' : int = 
        match x',y' with 
          [],[] -> 0
        | [], xs -> -1
        | xs, [] -> 1
        | x::xs, y::ys -> 
	  (match Base.compare x y with
	      0 -> compare_dna xs ys
            | other -> other)
	    

    (* implements a lexicographic ordering: 
     * compare the second components only if first components are equal *)
    let compare (a, b) (c, d) =
      match compare_dna a c with
	  0 -> compare_dna b d
        | other -> other
     
end;;

(* Task 4.4 *)

(* implement fast_lcs using your automatic memoizer functor! 
 * doing so will of course require proper creation of modules and
 * use of functors *)
let fast_lcs (ds : dna * dna) : dna =  failwith "unimplemented";;

(* Task 4.5 *)

(* Implement some experiment that shows performance difference
 * between slow_lcs and fast_lcs. (Print your results.)     
 * Explain in a brief comment what your experiment shows.        *)
Random.self_init

let test_slow (s1,s2:dna*dna) : float = 
	time_fun (fun _ -> (slow_lcs (s1,s2))) ()

let test_fast (s1,s2:dna*dna) : float = 
	time_fun (fun _ -> (fast_lcs (s1,s2))) ()

let print_header () =
  print_string "-----   Test 1   Test 2   \n";
  print_string "  N      Slow     Fast    \n";
  print_string "-----   ------   ------   \n"
;;

let print_row n test_slow test_fast =
  let space () = print_string "  " in
  let float f = Printf.printf "%6.4f" f in
  if n < 10000 then print_string " ";
  print_int n;       
	space (); space();
  float test_slow;
  space (); space ();
  float test_fast; 
	print_newline()

let experiment (n:int) : unit =
	let random_base = 
		let r = Random.int 4 in
		match r with 
		| 0 -> Base.A
		| 1 -> Base.C
		| 2 -> Base.T
		| 3 -> Base.G
		| _ -> failwith "impossible"
	in
	let rec generate_dna n = 
		match n with 
		| 0 -> []
		| _ -> random_base :: generate_dna (n-1)
  in
	let dna1 = generate_dna n in
	let dna2 = generate_dna n in
  print_row n (test_slow (dna1,dna2)) (test_fast (dna1,dna2))

let main () =
  let ns = [1000; 2000; 4000; 8000; 16000; 32000] in
  print_header();
  List.iter experiment ns
;;

(*

(* uncomment this block to run your experiment, 
 * but please do not submit with it uncommented
 *)
main ();;
*)

